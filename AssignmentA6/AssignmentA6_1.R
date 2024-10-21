rm(list = ls())
graphics.off()
cat("\014")

setwd("~/Documents/RStudio/TMS150 - Stochastic Data Processing/AssignmentA6")

library(stats)
library(tidyverse)

# Question 1 ----
# Set constants
n = 10000
theta = 13 # Limit support
# Parameters supported or not:
mu = 15
gamma = 1
# Utility
b1 = 10 # Support & Successful
b2 = -6 # Support and Unsuccessful
b3 = -1 # Abandoned

# Ensure replicability
#set.seed() #TODO: Needed?
# TODO:
x = seq(from = 0, to = 35, by = 0.1)
distr_gamma <- dgamma(x, shape = 2.5, rate = 0.25)

random_x <- sample(x, size = n, replace = TRUE, prob = distr_gamma)

# Calculate probability that the product is successful:
prob_successful <- exp((random_x - mu) * exp(-gamma))/
                       (1 + exp((random_x - mu) * exp(-gamma)))
# TODO: may use p() from next question

# Randomly decide if the product is successful, using probability of success
success <- as.logical(rbinom(n, size = 1, prob = prob_successful))

# Create tibble/dataframe, easier to handle
df_random <- tibble("random_x" = random_x, 
                    "prob_successful" = prob_successful, 
                    "success" = success)

# Decide which products are supported using limit
df_random <- mutate(df_random, "supported" = random_x>theta)

# Add the utility for each product
df_random <- df_random %>%
  mutate(utility = case_when(supported & success ~ b1,
                             supported & !success ~ b2,
                             !supported ~ b3))

# Calculate the utilities, using the theta chosen
utility_average <- df_random %>%
  select(utility) %>%
  sum()/n


cat("Average utility when theta =", theta, "is", utility_average, "\n")

df_random

# Question 1. c
theta = 10:20
utility_average = rep(0, length(theta))
for (i in 1:length(theta)) {
  # Decide which products are supported using limit
  df_random <- mutate(df_random, "supported" = random_x > theta[i])
  
  # Add the utility for each product
  df_random <- df_random %>%
    mutate(utility = case_when(supported & success ~ b1,
                               supported & !success ~ b2,
                               !supported ~ b3))
  
  # Calculate the sum of utilities for the action of supporting the products,
  # using the theta chosen
  utility_average[i] <- df_random %>%
    select(utility) %>%
    sum()/n
}

plot_df = tibble(Theta = theta, Utility = utility_average)
utility_average

ggplot(plot_df, aes(Theta, Utility))+
  geom_point()+
  geom_line()+
  labs(title = "Question 1.c")
ggsave("figures/q1c.png")

# Question 2 ----
rm(list = ls())
n = 10000
b1 = 10 # Support & Success
b2 = -6 # Support and Fail
b3 = -1 # Not support
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
ggsave("figures/q2.png")