# This script sequentially estimates the BGVAR model with 
# different parameters. Due to the computationally intensive nature of the task it 
# is recommended to run the parallelized version of this script: 
# bin/rscripts/estimate_bgvar_model.r


library(BGVAR)

# Load the data
bwList <- readRDS("../../data/tidy/bwList.RDS")
endoList <- readRDS("../../data/tidy/endoList.RDS")
exoList <- readRDS("../../data/tidy/exoList.RDS")
var.list <- readRDS("../../data/tidy/var_list.RDS")

params <- list(list(p=7, q=9))

# Define the function to estimate the model
estimateModel <- function(p, q, endogenous, exogenous, weights, variables) {
  model <- bgvar(Data = endogenous, 
                 Ex = exogenous,
                 W = weights,
                 plag = c(p, q),
                 draws=100, burnin=100, prior="SSVS", SV=TRUE, 
                 hold.out = 30,
                 eigen = 1,
                 expert = list(cores=4,
                               variable.list = variables) #specifies which variable is weakly exogenous
  )
  return(model)
}

# Initialize the progress bar
pb <- txtProgressBar(min = 0, max = length(params), style = 3)

# Time the execution of the code
timing <- system.time({
  for (i in 1:length(params)) {
    p_lag <- params[[i]]$p
    q_lag <- params[[i]]$q
    
    model_name <- paste0("../../models/model", p_lag, "_", q_lag, ".RDS")
    fcast_name <- paste0("../../models/model", p_lag, "_", q_lag, "_forecast_n30.RDS")
    
    # Update the progress bar with the current iteration number
    setTxtProgressBar(pb, i)
    
    model <- estimateModel(p_lag, q_lag, endoList, exoList, bwList, var.list)
    fcast <- predict(model, n.ahead = 30)
    
    print(paste0("Saving model with parameters p=", p_lag, " and q=", q_lag, " to ", model_name))
    # Save the model
    saveRDS(model, file = model_name)
    print(paste0("Saving model predictions to ", fcast_name))
    # Save the forecast
    saveRDS(fcast, file = fcast_name)
  }
})

# Convert the elapsed time to minutes and seconds
mins <- as.integer(timing["elapsed"] %/% 60)
secs <- as.integer(timing["elapsed"] %% 60)

# Print the timing in minutes and seconds
cat("Execution time:", mins, "minutes", secs, "seconds")
