library(scales)
library(ggplot2)

log_likelihood_df <- read.csv("../../results/3d_scatterplot_input.csv")

### Visualize the p/q lag parameter search
visualize_parameter_search <- function(df) {
  # treat p and q lags as discrete variables
  df$p_lag <- factor(df$p_lag)
  df$q_lag <- factor(df$q_lag)
  
  ll_plot <- ggplot(df,aes(x = p_lag, y = -log_likelihood, 
                                          color = q_lag)) + 
    theme_bw() +
    geom_point(size = 2, alpha =  0.5) +
    geom_line(aes(group = q_lag),size = 1.5) + 
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    labs( x = "p", 
          y = "Negative Log Likelihood", color = "q") +
    theme(axis.title = element_text(size = 30),
          axis.text = element_text(size = 25),
          legend.title = element_text(size = 30),
          legend.key.size = unit(2, 'cm'),
          legend.text = element_text(size = 25))
  
  ggsave(filename = "../../figures/log_likelihood_param_search.png", 
         plot = ll_plot,
         width = 10, height = 10,
         device = "png")
  
  plot(ll_plot)
}

visualize_parameter_search(log_likelihood_df)


