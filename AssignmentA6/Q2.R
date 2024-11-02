# -------------
# NOT USED BROR
# -------------

rm(list = ls())
graphics.off()
cat("\014")

setwd("~/Documents/RStudio/TMS150 - Stochastic Data Processing/AssignmentA6")

library(stats)
library(tidyverse)

thetas = 10:20
mu = 15
gamma = 1

# Empty array for final utilities, one per theta
utilities_thetas <- rep(0, length(thetas))

# Checked below function with previous question, seems ok
p <- function(x, mu, gamma){
  exp((x-mu)*exp(-gamma))/(1+exp((x-mu)*exp(-gamma)))
}

expected_utility <- function(theta, mu, gamma) {
  b1 = 10 # Support & Success
  b2 = -6 # Support and Fail
  b3 = -1 # Not support

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
# Repeat for all thetas in array thetas
for (i in 1:length(thetas)) {
  utilities_thetas[i] <- expected_utility(thetas[i], mu, gamma)
}

# Create df to plot
plot_df <- tibble(Theta = thetas,
                  Utility = utilities_thetas)

ggplot(plot_df, aes(x = Theta, y = Utility)) +
  geom_line()+
  geom_point()+
  labs(title = "Question 2",
       x = "Gamma",
       y = "Expected Utility")
