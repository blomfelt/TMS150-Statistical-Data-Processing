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


cat("\nQuestion 1\nAverage utility when theta =", theta, "is", utility_average, "\n\n")

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
       x = "Theta",
       y = "Expected Utility")
ggsave("figures/q2.png")

# Question 3a ----
# Read in the data:
successes <- read_csv("successful.txt", col_names = FALSE, show_col_types = FALSE)
unsuccesses <- read_csv("unsuccessful.txt", col_names = FALSE, show_col_types = FALSE)
# Assign correct Z value
success_df <- tibble("x" = successes$X1, success = 1)
unsuccess_df <- tibble(x = unsuccesses$X1, success = 0)
# Combine to one dataframe
data_df <- bind_rows(success_df, unsuccess_df)

# Define function 15
p <- function(x, mu, gamma){
  exp((x-mu)*exp(-gamma))/(1+exp((x-mu)*exp(-gamma)))
}

data_df <- data_df %>%
  mutate(mu_10_gamma_1 = p(x, 10, 1),
         mu_5_gamma_1  = p(x, 5, 1),
         mu_15_gamma_1 = p(x, 15, 1),
         mu_20_gamma_1 = p(x, 20, 1),
         mu_10_gamma_2 = p(x, 10, 2),
         mu_10_gamma_minus_1 = p(x, 15, -1),
         mu_20_gamma_minus_1 = p(x, 20, -1),
         mu_10_gamma_0 = p(x, 10,  0),
         mu_15_gamma_0 = p(x, 15,  0),)

ggplot(data_df, aes(x = x))+
  geom_point(aes(y = success))+
  geom_line(aes(y = mu_5_gamma_1, color = "mu_5_gamma_1"))+
  geom_line(aes(y = mu_10_gamma_1, color = "mu_10_gamma_1"))+
  geom_line(aes(y = mu_15_gamma_1, color = "mu_15_gamma_1"))+
  geom_line(aes(y = mu_20_gamma_1, color = "mu_20_gamma_1"))+
  geom_line(aes(y = mu_10_gamma_2, color = "mu_10_gamma_2"), linetype = "dashed")+
  geom_line(aes(y = mu_10_gamma_minus_1, color = "mu_15_gamma_minus_1"), linetype = "dotdash")+
  geom_line(aes(y = mu_15_gamma_0, color = "mu_15_gamma_0"), linetype = "dotted")+
  labs(title = "Question 3a",
       subtitle = "Solid: gamma = 1, dashed: gamma = 2, dotted: gamma = 0, dotdash: gamma = -1\nLeftmost solid: mu = 5")
ggsave("figures/q3a.png")
# Gamma should be around 1, certainly not 2, not as small as 0
# Mu should be somewhere between 10 and 15, gamma = 5 does not mat probability for the lower x:es, 
# and gamma = 20 does not match the middle ones. 


# Question 3b ----
# Since mu and gamma have flat priors, their log-prior is a constant which can 
# be ignored when computing the posterior
# Posterior is proportional to likelihood * prior, and prior is constant -> 
# -> log of posterior is just log-likelihood
# AND: The logarithm of a product is equal to a sum of logarithms
# May sum the log() of each value (x?)

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
successes <- read_csv("successful.txt", col_names = FALSE, show_col_types = FALSE)
unsuccesses <- read_csv("unsuccessful.txt", col_names = FALSE, show_col_types = FALSE)
# Assign correct Z value
success_df <- tibble("x" = successes$X1, success = 1)
unsuccess_df <- tibble(x = unsuccesses$X1, success = 0)
# Combine to one dataframe
data_df <- bind_rows(success_df, unsuccess_df)

# Print logpost of the chosen mu and gamma
cat("\nQuestion 3b\nLogpost, mu = 15, gamma = 1\n",logpost(15, 1, data_df$x, data_df$success))


# Question 3c ----
rm(list = ls())
# Function from before:
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
successes <- read_csv("successful.txt", col_names = FALSE, show_col_types = FALSE)
unsuccesses <- read_csv("unsuccessful.txt", col_names = FALSE, show_col_types = FALSE)
# Assign correct Z value
success_df <- tibble("x" = successes$X1, success = 1)
unsuccess_df <- tibble(x = unsuccesses$X1, success = 0)
# Combine to one dataframe
data_df <- bind_rows(success_df, unsuccess_df)

logpost(15, 1, data_df$x, data_df$success)

interesting_function <- function(mu, gamma){
  logpost(mu, gamma, data_df$x, data_df$success)
}

interesting_mu = seq(10, 20, length.out = 10)
interesting_gamma = seq(0, 2.5, length.out = 20)

interesting_matrix <- outer(interesting_mu, interesting_gamma, Vectorize(interesting_function))

png("figures/q3c.png")
image(interesting_mu, interesting_gamma, interesting_matrix)
graphics.off()


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
successes <- read_csv("successful.txt", col_names = FALSE, show_col_types = FALSE)
unsuccesses <- read_csv("unsuccessful.txt", col_names = FALSE, show_col_types = FALSE)
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
  # Use 1/logpost since we want the value closest to zero (least negative/highest)
  1/logpost(mu, gamma, data_df$x, data_df$success)
}


optimum <-nlm(optimizing_function, c(13, 1))
# Extract values of mu and gamma
optimum_values <- optimum$estimate
# Extract minimum value, restore to real form by 1 divided by it
optimum_estimate <- 1/optimum$minimum

cat("\nQuestion 4\nOptimal value of mu:\t", optimum_values[1],
    "\nOptimal value of gamma:\t", optimum_values[2],
    "\nEstimate at these points:", optimum_estimate)
