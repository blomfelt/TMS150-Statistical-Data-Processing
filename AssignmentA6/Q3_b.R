# -------------
# NOT USED BROR
# -------------

rm(list = ls())
graphics.off()
cat("\014")

setwd("~/Documents/RStudio/TMS150 - Stochastic Data Processing/AssignmentA6")

library(stats)
library(tidyverse)

# Question 3b
# Since mu and gamma have flat priors, their log-prior is a constant which can 
# be ignored when computing the posterior
# Posterior is proportional to likelihood * prior, and prior is constant -> 
# -> log of posterior is just log-likelihood
# AND: The logarithm of a product is equal to a sum of logarithms
# May sum the log() of each value

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

# Test using data from before
successes <- read_csv("successful.txt", col_names = FALSE)
unsuccesses <- read_csv("unsuccessful.txt", col_names = FALSE)
# Assign correct Z value
success_df <- tibble("x" = successes$X1, success = 1)
unsuccess_df <- tibble(x = unsuccesses$X1, success = 0)
# Combine to one dataframe
data_df <- bind_rows(success_df, unsuccess_df)

# Print logpost of the chosen mu and gamma
cat("\nLogpost, mu = 15, gamma = 1\n",logpost(15, 1, data_df$x, data_df$success))