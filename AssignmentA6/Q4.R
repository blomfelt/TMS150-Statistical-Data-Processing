rm(list = ls())
graphics.off()
cat("\014")

setwd("~/Documents/RStudio/TMS150 - Stochastic Data Processing/AssignmentA6")

library(stats)
library(tidyverse)

# Question 4
rm(list = ls())
# Function from before
logpost <- function(mu, gamma, x, success){
  # Success = 1 or 0 for each x
  
  # Same probability function as before
  p <- function(x, mu, gamma){
    exp((x-mu)*exp(-gamma))/(1+exp((x-mu)*exp(-gamma)))
  }
  
  # Calculate the prob of success
  p_success <- p(x, mu, gamma)
  
  # Log likelihood of dataset
  # log(p) for successes, and log(1-p) for unsucceses
  log_likelihood <- success * log(p_success) + (1 - success) * log(1 - p_success)
  
  # Posterior is proportional to likelihood * prior, and prior is constant -> 
  # -> log of posterior is just log-likelihood
  return(sum(log_likelihood))
  # Sum of log = log of product
}

# Data from before
successes <- read_csv("successful.txt", col_names = FALSE)
unsuccesses <- read_csv("unsuccessful.txt", col_names = FALSE)
# Assign correct Z value
success_df <- tibble("x" = successes$X1, success = 1)
unsuccess_df <- tibble(x = unsuccesses$X1, success = 0)
# Combine to one dataframe
data_df <- bind_rows(success_df, unsuccess_df)


# Same as previous also
interesting_function_old <- function(mu, gamma){
  logpost(mu, gamma, data_df$x, data_df$success)
}
# Modified from previous question
optimizing_function <- function(values){
  mu = values[1]
  gamma = values[2]
  # Use -1*logpost since we want the  highest value of the densities
  -logpost(mu, gamma, data_df$x, data_df$success)
}


optimum <-nlm(optimizing_function, c(13, 1))
# Extract values of mu and gamma
optimum_values <- optimum$estimate
# Extract minimum value, maximum by -1*minimum
optimum_estimate <- -optimum$minimum

cat("\nQuestion 4\nOptimal value of mu:\t", optimum_values[1],
    "\nOptimal value of gamma:\t", optimum_values[2],
    "\nEstimate at these points:", optimum_estimate)
