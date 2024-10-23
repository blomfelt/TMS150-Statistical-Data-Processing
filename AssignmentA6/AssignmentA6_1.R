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
set.seed(321)

# Create the array of values of x and the gamma distribution
x = seq(from = 0, to = 1000, by = 0.1)
distr_gamma <- dgamma(x, shape = 2.5, rate = 0.25)

# Randomly sample from the distribution
random_x <- sample(x, size = n, replace = TRUE, prob = distr_gamma)

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

# Calculate the utilities, using the theta chosen
utility_average <- df_random %>%
  select(utility) %>%
  sum()/n

cat("\nQuestion 1\nAverage utility when theta =", theta, 
    "is", utility_average, "\n\n")


# Question 1c
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

# Create dataframe to plot it with ggplot
plot_df = tibble(Theta = theta, Utility = utility_average)

# Plot it and save it
ggplot(plot_df, aes(Theta, Utility))+
  geom_point()+
  geom_line()+
  labs(title = "Question 1.c")
ggsave("figures/q1c.png", width = 6, height = 4)


# Question 2 ----
rm(list = ls())
# Set constants
thetas = 10:20
mu = 15
gamma = 1

# Empty array for final utilities, one per theta
utilities_thetas <- rep(0, length(thetas))

expected_utility <- function(theta, mu, gamma) {
  # Utility of x, gets theta from 'expected_utility' input
  utility_x <- function(x) {
    b1 = 10 # Support & Success
    b2 = -6 # Support and Fail
    b3 = -1 # Not support
    
    # Calculate probability of success for the current x
    # Gets mu and gamma from 'expected_utility', not elegant but works
    p_success <- exp((x-mu)*exp(-gamma))/(1+exp((x-mu)*exp(-gamma)))
    
    # Calculate expected utility if supported
    supported_utility <- b1 * p_success + b2 * (1 - p_success)
    
    # Return the correct utility value
    return(ifelse(x >= theta, supported_utility, b3))
  }
  
  # Integrate over whole dgamma
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

# Plot it and save it 
ggplot(plot_df, aes(x = Theta, y = Utility)) +
  geom_line()+
  geom_point()+
  labs(title = "Question 2",
       x = "Theta",
       y = "Expected Utility")
ggsave("figures/q2.png", width = 6, height = 4)


# Question 3a ----
rm(list = ls())
# Read in the data:
successes <- read_csv("successful.txt", 
                      col_names = FALSE, show_col_types = FALSE)
unsuccesses <- read_csv("unsuccessful.txt", 
                        col_names = FALSE, show_col_types = FALSE)

# Assign correct Z value
success_df <- tibble("x" = successes$X1, success = 1)
unsuccess_df <- tibble(x = unsuccesses$X1, success = 0)

# Combine to one dataframe
data_df <- bind_rows(success_df, unsuccess_df)

# Define function 15
p <- function(x, mu, gamma){
  exp((x-mu)*exp(-gamma))/(1+exp((x-mu)*exp(-gamma)))
}

# Create dataframt to plot
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

# Plot it and save it
ggplot(data_df, aes(x = x))+
  geom_point(aes(y = success))+
  geom_line(aes(y = mu_5_gamma_1, color = "mu_5_gamma_1"))+
  geom_line(aes(y = mu_10_gamma_1, color = "mu_10_gamma_1"))+
  geom_line(aes(y = mu_15_gamma_1, color = "mu_15_gamma_1"))+
  geom_line(aes(y = mu_20_gamma_1, color = "mu_20_gamma_1"))+
  geom_line(aes(y = mu_10_gamma_2, color = "mu_10_gamma_2"), 
            linetype = "dashed")+
  geom_line(aes(y = mu_10_gamma_minus_1, color = "mu_15_gamma_minus_1"), 
            linetype = "dotdash")+
  geom_line(aes(y = mu_15_gamma_0, color = "mu_15_gamma_0"), 
            linetype = "dotted")+
  labs(title = "Question 3a",
       subtitle = paste("Solid: gamma = 1, dashed: gamma = 2, ",
                        "\nDotted: gamma = 0, dotdash: gamma = -1",
                        "\nLeftmost solid: mu = 5"),
       y = "Success", 
       color = "Color")
ggsave("figures/q3a.png", width = 6, height = 4)
# Gamma should be around 1, certainly not 2, not as small as 0
# Mu should be somewhere between 10 and 15, gamma = 5 does not match probability 
# for the lower x:es, and gamma = 20 does not match the middle ones. 


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
  # log(p) for successes, and log(1-p) for unsuccesses
  log_likelihood <- success * log(p_success) + (1 - success) * log(1 - p_success)
  
  # Posterior is proportional to likelihood * prior, and prior is constant -> 
  # -> log of posterior is just log-likelihood
  return(sum(log_likelihood))
  # Sum of log = log of product
}

# Test using data from before
successes <- read_csv("successful.txt", 
                      col_names = FALSE, show_col_types = FALSE)
unsuccesses <- read_csv("unsuccessful.txt", 
                        col_names = FALSE, show_col_types = FALSE)
# Assign correct Z value
success_df <- tibble("x" = successes$X1, success = 1)
unsuccess_df <- tibble(x = unsuccesses$X1, success = 0)
# Combine to one dataframe
data_df <- bind_rows(success_df, unsuccess_df)

# Print logpost of the chosen mu and gamma
cat("\nQuestion 3b\nLogpost, mu = 15, gamma = 1\n",
    logpost(15, 1, data_df$x, data_df$success))


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
  # log(p) for successes, and log(1-p) for unsuccesses
  log_likelihood <- success * log(p_success) + (1 - success) * log(1 - p_success)
  
  # Posterior is proportional to likelihood * prior, and prior is constant -> 
  # -> log of posterior is just log-likelihood
  return(sum(log_likelihood))
  # Sum of log = log of product
}

# Data from before
successes <- read_csv("successful.txt", 
                      col_names = FALSE, show_col_types = FALSE)
unsuccesses <- read_csv("unsuccessful.txt", 
                        col_names = FALSE, show_col_types = FALSE)
# Assign correct Z value
success_df <- tibble("x" = successes$X1, success = 1)
unsuccess_df <- tibble(x = unsuccesses$X1, success = 0)
# Combine to one dataframe
data_df <- bind_rows(success_df, unsuccess_df)

# Test the function:
#logpost(15, 1, data_df$x, data_df$success)

# Define new wrapper to work with 'Vectorize' and 'outer'
interesting_function <- function(mu, gamma){
  logpost(mu, gamma, data_df$x, data_df$success)
}

# Set the range of interesing values for mu and gamma
interesting_mu = seq(10, 20, length.out = 100)
interesting_gamma = seq(0, 2, length.out = 100)

# Calculate the logarithm of the posterior density for every combination of the 
# two ranges of interesting values.
interesting_matrix <- outer(interesting_mu, interesting_gamma, 
                            Vectorize(interesting_function))

# Plot and save
png("figures/q3c.png", width = 4, height = 4, units = "in", res = 300)
image(interesting_mu, interesting_gamma, interesting_matrix, 
      xlab = "Mu", ylab = "Gamma")
title("Question 3c")
graphics.off()

# Zoom in
interesting_mu = seq(13, 18.1, length.out = 100)
interesting_gamma = seq(0.7, 2, length.out = 100)
# Calc log(post)
interesting_matrix <- outer(interesting_mu, interesting_gamma, 
                            Vectorize(interesting_function))
# Plot and save
png("figures/q3c_zoom.png", width = 4, height = 4, units = "in", res = 300)
image(interesting_mu, interesting_gamma, interesting_matrix, 
      xlab = "Mu", ylab = "Gamma")
title("Question 3c, zoomed")
graphics.off()


# Question 4 ----
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
  # log(p) for successes, and log(1-p) for unsuccesses
  log_likelihood <- success * log(p_success) + (1 - success) * log(1 - p_success)
  
  # Posterior is proportional to likelihood * prior, and prior is constant -> 
  # -> log of posterior is just log-likelihood
  return(sum(log_likelihood))
  # Sum of log = log of product
}

# Data from before
successes <- read_csv("successful.txt", 
                      col_names = FALSE, show_col_types = FALSE)
unsuccesses <- read_csv("unsuccessful.txt", 
                        col_names = FALSE, show_col_types = FALSE)
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

# Optimize, using start values
optimum <-nlm(optimizing_function, c(14, 1))
# Extract values of mu and gamma
optimum_values <- optimum$estimate
# Extract minimum value, maximum by -1*minimum
optimum_estimate <- -optimum$minimum

cat("\nQuestion 4\nOptimal value of mu:\t", optimum_values[1],
    "\nOptimal value of gamma:\t", optimum_values[2],
    "\nEstimate at these points:", optimum_estimate)


# Question 5 ----
rm(list = ls())

# From previous question
optimal_mu <- 14.459681
optimal_gamma <- 1.044977

# From question 2
expected_utility <- function(theta, mu, gamma) {
  # Utility of x, gets theta from 'expected_utility' input
  utility_x <- function(x) {
    b1 = 10 # Support & Success
    b2 = -6 # Support and Fail
    b3 = -1 # Not support
    
    # Calculate probability of success for the current x
    # Gets mu and gamma from 'expected_utility', not elegant but works
    p_success <- exp((x-mu)*exp(-gamma))/(1+exp((x-mu)*exp(-gamma)))
    
    # Calculate expected utility if supported
    supported_utility <- b1 * p_success + b2 * (1 - p_success)
    
    # Return the correct utility value
    return(ifelse(x >= theta, supported_utility, b3))
  }
  
  # Integrate over whole dgamma
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

cat("\n\nQuestion 5\nOptimal value of theta:", optimum_theta,
    "\nExpected utiity at this point:", optimum_expected_utility)
