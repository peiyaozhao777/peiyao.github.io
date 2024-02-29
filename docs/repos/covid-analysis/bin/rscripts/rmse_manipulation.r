# set working directory (setwd) to ../covid-analysis/bin/rscripts
# load libraries
library(BGVAR) 
library(tidyr)
library(tibble)
library(dplyr)
library(ggplot2)


# load forecast with lag params p=7, q=9 based on optimal model
fcast7.9 <- readRDS("../../models/double_check/model7_9_forecast_n30.RDS")

generate_rmse_plot <- function(bgvar_forecast, pq_lag='p_q_lag', save_file = FALSE) {
  rmse <- rmse(bgvar_forecast)
  
  rmse.df <- as.data.frame(rmse[30, 1:260])
  rmse.convert.index <- rownames_to_column(rmse.df, var="index")
  
  # separate the country and category from the index
  rmse.convert.index$country <- sub("\\..*", "", rmse.convert.index$index)
  rmse.convert.index$category <- sub(".*\\.", "", rmse.convert.index$index)
  
  # pivot the data so that the index becomes separate columns
  rmse.wide <- pivot_wider(rmse.convert.index, names_from = category, values_from = `rmse[30, 1:260]`)
  
  # select and rename the columns
  rmse.final <- rmse.wide %>% select(country, cases, grocery, residential, transit, workplaces)
  names(rmse.final)[2:6] <- c("Cases", "Grocery", "Home", "Transit", "Work")
  
  # consolidate rows into a single row
  rmse.final <- rmse.final %>% 
    group_by(country) %>% 
    summarize_all(funs(max(., na.rm = TRUE))) %>% 
    ungroup()
  
  # Calculate the mean RMSE value of each endogenous variable
  avg_values <- sapply(rmse.final[,2:6], mean)
  med_values <- sapply(rmse.final[,2:6], median)
  # Print the results
  print("Mean RMSE")
  print(avg_values)
  print("Median RMSE")
  print(med_values)
  write.csv(rmse.final, file="../../results/rmse.csv")
  
  # convert the data frame from wide to long format, 
  df_long <- gather(rmse.final, variable, value, -country)
  
  #if(pq_lag == "p_q_lag") {
  #  parsed_title <- ""  
  #} else {
  #  # Split the string into two parts using the underscore as the delimiter
  #  parts <- strsplit(pq_lag, "_")[[1]]
  #  # Extract the values for p_lag and q_lag from the parts
  #  p_lag <- parts[2]
  #  q_lag <- parts[3]
  #  
  #  # Create the output string
  #  parsed_title <- paste0("p_lag = ", p_lag, ", q_lag = ", q_lag)
  #}
  
  # create a new column in df_long with the labels for each variable
  df_long$label <- ifelse(df_long$variable == "Cases", "New daily COVID-19 cases", "Pcnt Change from baseline")

  # use the new label column in scale_y_continuous
  rmse_facet <- ggplot(df_long, aes(x = country, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ variable + label, nrow = 5, scales = "free_y", 
               labeller = labeller(label = function(x) 
                 ifelse(x == "New daily COVID-19 cases", "New daily cases", "(% change)")),
               strip.position = "right") +
    scale_fill_manual(values=c("darkblue","darkred","lightblue","pink","darkgreen")) +
    labs(color = "", fill = "", x = "Country", y = "") + #title = parsed_title,
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16),
          axis.text.y = element_text(size = 16),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          strip.text.y = element_text(size = 16, margin = margin(t = 1, r = 0, b = 0, l = 0)),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "white"))#,
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank()) 
  
  
  if(save_file) {
    #save_as <- paste0("../../figures/rmse_facet.png")
    ggsave(filename = "../../figures/rmse_facet.png",
           plot = rmse_facet,
           width = 15, height = 10,
           device = "png")  
  }
  
  plot(rmse_facet)
}

generate_rmse_plot(fcast7.9)
