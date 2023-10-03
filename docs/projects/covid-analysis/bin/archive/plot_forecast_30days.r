library(BGVAR)
library(reshape2)
library(ggplot2)
library(RColorBrewer)


# load optimal model and forecast
model_opt <- readRDS("../../models/model7_9.RDS")
fcast <- readRDS("../../models/predictions/model7_9_forecast_n30.RDS")

forecast7.9 <- as.data.frame(fcast$fcast)
# get 5th percentile
pred.Q5 <- as.data.frame(forecast7.9[, grep("\\.Q5$", names(forecast7.9))])
pred.median <- as.data.frame(forecast7.9[, grep("\\.Q50$", names(forecast7.9))])
# get 95th percentile
pred.Q95 <- as.data.frame(forecast7.9[, grep("\\.Q95$", names(forecast7.9))])


# last 30 days forecast (95% credible interval)
pred.Q5.transposed <- as.data.frame(t(pred.Q5))

pred.Q95.transposed <- as.data.frame(t(pred.Q95))

# Add a new column with the days
pred.Q5.transposed$day <- seq_len(nrow(pred.Q5.transposed))
pred.Q95.transposed$day <- seq_len(nrow(pred.Q95.transposed))

# Reorder columns
pred.Q5.transposed <-  pred.Q5.transposed[, c("day", rownames(pred.Q5))]
pred.Q95.transposed <- pred.Q95.transposed[, c("day", rownames(pred.Q95))]

# Filter out variables associated with "DE"
pred.Q5.DE <- pred.Q5.transposed[, grep("DE", colnames(pred.Q5.transposed))]
pred.Q95.DE <- pred.Q95.transposed[, grep("DE", colnames(pred.Q95.transposed))]


# Rename columns
# colnames(data_Q95_transposed)[-1] <- gsub("\\.Q95$", "", colnames(data_Q95_transposed)[-1])


pred.Q5.DE <- as.data.frame(pred.Q5.DE)
pred.Q5.DE$day <- 1:nrow(pred.Q5.DE)
pred.Q95.DE <- as.data.frame(pred.Q95.DE)
pred.Q95.DE$day <- 1:nrow(pred.Q95.DE)


# get observed data from model
observed <- model_opt$args$Data$DE
obs_df <- as.data.frame(observed)
obs_df$day <- 1:nrow(obs_df)

obs_df_last30 <- tail(obs_df, 30)

obs_long <- gather(obs_df_last30, variable, value, -day)

obs_df_last30$day <- 1:30

data_merged <- merge(obs_df_last30, pred.Q95.DE, by = "day")

# Convert the merged data from wide to long format
data_long <- tidyr::pivot_longer(data_merged, -day, names_to = "variable", values_to = "value")

#brew_colors[1]
brew_colors <- brewer.pal(12, name = "Set3")

ggplot(data_long, aes(x = day, y = value, color = variable)) +
  geom_line(aes(group = variable)) +
  facet_wrap(~ variable, scales = "free_y", nrow = 5) +
  theme_bw() +
  labs(x = "Day", y = "Value", color = NULL) +
  scale_color_manual(values = brew_colors) # set the colors for observed and predicted data

vals <- c("cases" = brew_colors[1],
          "residential" = brew_colors[2], 
          "workplaces" = brew_colors[3], 
          "transit" = brew_colors[4], 
          "grocery" = brew_colors[5])

ggplot(obs_long, aes(x = day, y = value, color = variable)) +
  geom_line() +
  ggtitle("DE observed") +
  labs(x = "Day", y = "") +
  scale_color_manual(values = vals) +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ variable, 
             nrow = 5,
             scales = "free_y") 

# Reshape the data to long format
data_Q95_DE_long <- gather(pred.Q95.DE, variable, value, -day)

# Create a stacked bar chart with facet_wrap
ggplot(data_Q95_DE_long, aes(x = day, y = value, color = variable)) +
  geom_line() +
  ggtitle("DE predicted") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~variable, nrow = 5, scales = "free_y")
