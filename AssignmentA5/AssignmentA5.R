rm(list = ls())
graphics.off()
cat("\014")

setwd("~/Documents/RStudio/TMS150 - Stochastic Data Processing/AssignmentA5/")

# Question 3
# Create empty matrix of the correct size
values <- matrix(nrow = 6, ncol = 6)
for (i in 1:6){
  for (j in 1:6){
    # x*y*f(x)*f(y) where f(x)*f(y) = 1/6*1/6 = 1/36
    values[i, j] <- i * j / 36
  }
}

cat("\nQuestion 3\nExpectation value:", sum(values))



# Question 4
cat("\n\nQuestion 4\n")
# Set constants
n <- 10 # Number of steps
max_x <- 1 # Upper limit of integration
min_x <- -1 # Lower limit
# Step size of x
x_step <- (max_x - min_x) / n
# Create empty vector of correct size
phi <- vector(length = n)

for (i in 1:n){
  # Calculate the current x, left side of box
  x <- min_x + (i - 1) * x_step
  # Calculate the area of the box and save
  phi[i] <- (1 / (sqrt(2 * pi)) * exp(-1 * x^2 / 2)) * x_step
}
# Sum all the areas and print them
cat("Sum:\t ", sum(phi))
# Use built-in function
integration_function <- integrate(dnorm, -1, 1)$value
cat("\nFunction:", integration_function)
cat("\nRatio sum/fun:", integration_function / sum(phi))

# Question 5
cat("\n\nQuestion 5\n")

# Set constants
num_loop <- 20
timesteps <- seq(0, 1, 0.01)

png("figures/q5.png")
# Create an empty plot to be able to use 'lines' in the loop, 
# limits set manually so every line is included
plot(timesteps, exp(-timesteps * 0.5) * 5, type = "n",
     xlim = c(0, 1), ylim = c(0, 10),
     xlab = "t", ylab = "u(t)")

# Set seed for reproducibility
set.seed(321)
for (i in 1:num_loop){
  # Sample x and y for each iteration
  x <- rnorm(1, mean = 5, sd = 1.5)
  y <- rnorm(1, mean = 0.5, sd = 0.3)
  # Plot the line
  lines(timesteps, exp(-timesteps * y) * x)
}
graphics.off()


# b)

# Set constants
t <- 1
n <- 500

a <- 0
b <- 10
c <- -1
d <- 2

mu_x <- 5
sigma_x2 <- 1.5^2
mu_y <- 0.5
sigma_y2 <- 0.3^2

# Vectors of values for x and y in the ranges defined above
x_values <- seq(from = a, to = b, length.out = n)
y_values <- seq(from = c, to = d, length.out = n)

# Calculate the step size
stepsize_x <- (b - a) / n
stepsize_y <- (d - c) / n

# Calculate the necessary values, n in total for each
# Use the functionality of R to do it once for each value in the array, the
# left-hand-side of the box with width stepsize_x and stepsize_y respectively
fx <- 1 / (sqrt(2 * pi * sigma_x2)) * exp(-(x_values - mu_x)^2 / (2 * sigma_x2))
fy <- 1 / (sqrt(2 * pi * sigma_y2)) * exp(-(y_values - mu_y)^2 / (2 * sigma_y2))

# Create empty matrix
value <- matrix(nrow = n, ncol = n)
for (i in 1:n){
  for (j in 1:n){
    # Calculate every value of h for every combination of x and y in the matrix
    h <- exp(-y_values[j]) * x_values[i]
    # Calculate the size of the box and save
    value[i, j] <- h * fx[i] * fy[j] * stepsize_x * stepsize_y
  }
}
cat("Sum:", sum(value))
