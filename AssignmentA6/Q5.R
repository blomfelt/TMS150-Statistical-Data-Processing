rm(list = ls())
graphics.off()
cat("\014")

setwd("~/Documents/RStudio/TMS150 - Stochastic Data Processing/AssignmentA6")

library(stats)
library(tidyverse)

# Question 5
rm(list = ls())

# From previous question
optimal_mu <- 14.459681
optimal_gamma <- 1.044977

# From question 2
expected_utility <- function(theta, mu, gamma) {
  # Added now:
  b1 = 10 # Support & Success
  b2 = -6 # Support and Fail
  b3 = -1 # Not support
  
  # Checked below function with previous question, seems ok
  p <- function(x, mu, gamma){
    exp((x-mu)*exp(-gamma))/(1+exp((x-mu)*exp(-gamma)))
  }
  
  # Utility of x
  utility_x <- function(x) {
    p_success <- p(x, mu, gamma)
    supported_utility <- b1 * p_success + b2 * (1 - p_success)
    abandoned_utility <- b3
    return(ifelse(x >= theta, supported_utility, abandoned_utility))
  }
  # Integrate over whole dgamma, upper = 1000 is overkill though
  integrate(function(x) {dgamma(x, shape = 2.5, rate = 0.25) * utility_x(x)}, 
            lower = 0, upper = 1000)$value
}

optimizing_function <- function(theta){
  # Same as above, from previous question
  optimal_mu <- 14.459681
  optimal_gamma <- 1.044977
  expected_utility(theta, optimal_mu, optimal_gamma)
}

# Optimize between 10 and 20, searching for the maximum
optimum <- optimize(optimizing_function, interval = c(10, 20), maximum = TRUE)
# Extract optimum theta
optimum_theta <- optimum$maximum
# Extract the expected utility at the optimum
optimum_expected_utility <- optimum$objective

cat("\nQuestion 5\nOptimal value of theta:", optimum_theta,
    "\nExpected utiity at this point:", optimum_expected_utility)
