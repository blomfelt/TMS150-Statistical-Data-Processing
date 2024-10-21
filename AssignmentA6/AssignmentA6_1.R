rm(list = ls())
graphics.off()
cat("\014")

setwd("~/Documents/RStudio/TMS150 - Stochastic Data Processing/AssignmentA6")

library(stats)
library(tidyverse)

# Question 1 ----
# Set constants
n = 1000
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
# Generate the random values of x:
random_x <- rgamma(n, shape = 2.5, rate = 0.25)

# Calculate probability that the product is successful:
prob_successful <- exp((random_x - mu) * exp(-gamma))/
                       (1 + exp((random_x - mu) * exp(-gamma)))

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

# Calculate the sum of utilities for the action of supporting the products, 
# using the theta chosen
utility_supported <- df_random %>%
  filter(supported)%>%
  select(utility) %>%
  sum()

# Calculate average utility for the chosen theta:
average_utility <- utility_supported/sum(df_random$supported)

cat("Average utility when theta =", theta, "is", average_utility, "\n")

df_random

# Question 1. c
theta = 10:20
average_utility = rep(0, length(theta))
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
  utility_supported <- df_random %>%
    filter(supported) %>%
    select(utility) %>%
    sum()
  
  # Calculate average utility for the chosen theta:
  average_utility[i] <- utility_supported / sum(df_random$supported)
  #cat("\nTheta =", theta[i], "Num supported =", sum(df_random$supported))
}

test_df = tibble("Theta" = theta, "Utility" = average_utility)
average_utility
#plot(theta, average_utility)
ggplot(test_df, aes(theta, Utility))+
  geom_point()+
  geom_line()+
  labs(title = "Question 1.c")
