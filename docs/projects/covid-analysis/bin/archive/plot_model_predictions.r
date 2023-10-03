library(BGVAR)
library(tidyr)
library(dplyr)
library(ggplot2)
library(latticeExtra)

model_opt <- readRDS("../../models/model7_9.RDS")
countries <- c("DE", "KR", "US", "ZA")

# Define the low and high hues of the color scale
low_hue <- 240  # blue
high_hue <- 0   # red

# Define the chroma and lightness of the colors
chroma <- 50
lightness <- seq(75, 35, length.out = 100)

# Create a custom blue-to-red color palette in the hcl color space
my_palette <- hcl(seq(low_hue, high_hue, length.out = 100), chroma, lightness)

# Display the color palette
my_palette

plot_model_pred <- function(model, country, save_file = FALSE) {
  c <- model$args$Data[[country]]
  df <- as.data.frame(c)
  df$day <- 1:nrow(df)
  vals <- c("cases" = "red",
            "residential" = "blue", 
            "workplaces" = "green", 
            "transit" = "purple", 
            "grocery" = "orange")
  data_long <- gather(df, variable, value, -day)
  file_name <- paste0("../../figures/model_predictions_",country,".png")
  axis_labels <- function(x) {
    if (x == "cases") {
      return("Change in daily COVID-19 cases")
    } else {
      return("Percent change from baseline")
    }
  }
  p <- ggplot(data_long, aes(x = day, y = value, color = variable)) +
    geom_line() +
    ggtitle(country) +
    labs(x = "Day", y = "") +
    scale_color_manual(values = vals) +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~ variable, 
               nrow = 5,
               scales = "free_y") 
  if(save_file){
    ggsave(file_name, p, width = 8, height = 10, device = "png") 
  }
  plot(p)
}

for(c in countries) {
  plot_model_pred(model_opt, c, save_file = FALSE)  
}

