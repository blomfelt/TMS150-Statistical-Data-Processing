rm(list = ls()); graphics.off(); cat("\014")

setwd("~/Documents/RStudio/TMS150 - Stochastic Data Processing/AssignmentA2a")

library(ggplot2)
library(MASS)

# To save all plots change to TRUE
savePlots <-  FALSE
closePlots <- TRUE

if(savePlots) closePlots <- TRUE

# Response variable y = brwt = brain weight
# Covariate x = bwt = body weight

# Exercise 1 ----
# 1.1
# Compute 2000 new observations, then calculate a 95% prediction intervals for 
# all 16 values of log(bwt) in [-5, -4, ..., 10]
# Produce a scatterplot and overlay the upper and lower bound you obtained. 
# You should see three observations outside the intervals. 
# - What do we conclude? 
# - Are these three observations outside the intervals by a considerable amount? 
# - Motivate
sleeptab <- read.table('sleeptab.dat',header=TRUE) # loads the dataset
# the option header = TRUE tells R that the first row
# contains the variables names
attach(sleeptab) # this way you can access the variables by name
# Inspect the data and show the structure of it:
head(sleeptab)

# Create a linear model of the correct form
ex1model <- lm(log(brwt)~log(bwt))
# Extract the coefficients
coeffex1 <- ex1model$coefficients
# Set the seed in preparation for the creation of the error term
set.seed(321)

# The equispaced values of log(bwt)
log_bwt <- -5:10 
# Empty matrix with the correct size:
predicted_brwt <- matrix(nrow = 2000, ncol = length(log_bwt)) 
for (B in 1:2000) {
  # The correct number of error terms, new for each iteration:
  error <- rnorm(length(log_bwt), 0, sigma(ex1model))
  
  # Manually create the predicted values using the error term. 
  # Size of log_bwt = size of error -> elementwise operations. 
  # Gives an array which we place on row B.
  predicted_brwt[B, ] <- coeffex1[1] + coeffex1[2]*log_bwt + error 
}

confidenceIntervals <- data.frame("low"=0, "high"=0) # Create an empty dataframe
for (i in 1:length(log_bwt)) {
  # Extract the limits of the confidence interval
  confidenceIntervals[i, ] <- quantile(predicted_brwt[,i], c(0.025, 0.975)) 
}
confidenceIntervals["logBWT"] <- log_bwt #Add so it is easier to plot

# Plot it and save it
ggplot()+
  geom_point(data = sleeptab, aes(x=log(bwt), y=log(brwt)))+#, shape = 1
  geom_line(data = confidenceIntervals, 
            aes(x = logBWT, y = low), linetype = "longdash")+
  geom_line(data = confidenceIntervals, 
            aes(x = logBWT, y = high), linetype = "longdash")+
  labs(title = "Log-transformed variables",
       subtitle = "With 95% confidence interval (dashed lines)")
if (savePlots) ggsave("figures/Ex1_1.png", 
                      width = 1770, height = 1000, units = "px")
if (closePlots) graphics.off()

# Find those outside the confidence interval (values guessed from plot)
sleeptab[as.logical((log(sleeptab$bwt) > 1) * (log(sleeptab$brwt) < 2)),]
# Wateropossum
sleeptab[as.logical((log(sleeptab$bwt) < 2) * (log(sleeptab$brwt) > 5)),]
# Rhesusmonkey
sleeptab[as.logical((log(sleeptab$bwt) < 5) * (log(sleeptab$brwt) > 7)),]
# Man


# 1.2 Same as above, but for nontransformed data and using bwt in 
# [0, 10, 20, ... 7000]
# What do we conclude in this case? 
# - Once more, here you find that some observations are outside the intervals. 
# - Are these outside by a considerable amount? 
# - Comment on your findings.

# Create the correct model
ex12model <- lm(brwt~bwt)
# Extract the coefficients
coeffex12 <- ex12model$coefficients
# Set set before the loop
set.seed(321)

new_bwt <- seq(0, 7000, 10) # The equispaced values of bwt
# Empty matrix with the correct size
predicted_brwt_nontrans <- matrix(nrow = 2000, ncol = length(new_bwt)) 
for (B in 1:2000) {
  # The correct number of error terms, new for each iteration
  error <- rnorm(length(new_bwt), 0, sigma(ex12model)) 
  # Manually create the predicted values using the error term. 
  # Size of log_bwt = size of error -> elementwise operations. 
  # Gives an array which we place on row B.
  predicted_brwt_nontrans[B, ] <- coeffex12[1] + coeffex12[2]*new_bwt + error 
}

# Create an empty data frame
confidenceIntervals_nontrans <- data.frame("low"=0, "high"=0) 
for (i in 1:length(new_bwt)) {
  # Extract the limits of the confidence interval
  confidenceIntervals_nontrans[i, ] <- quantile(predicted_brwt_nontrans[,i], 
                                                c(0.025, 0.975))
}
confidenceIntervals_nontrans["new_BWT"] <- new_bwt #Add so it is easier to plot

# Plot it and save it
ggplot()+
  geom_point(data = sleeptab, aes(x=bwt, y=brwt))
  geom_smooth(data = confidenceIntervals_nontrans, aes(x = new_BWT, y = low), 
              linetype = "dashed", col = "black")+
  geom_smooth(data = confidenceIntervals_nontrans, aes(x = new_BWT, y = high), 
              linetype = "dashed", col = "black")+
  labs(title = "Non-transformed variables",
       subtitle = "With 95% confidence interval (dashed lines)",
       y = "brwt [g]",
       x = "bwt [kg]")
if (savePlots) ggsave("figures/Ex1_2.png", 
                      width = 1770, height = 1000, units = "px")
if (closePlots) graphics.off()

# Find those outside the confidence interval (values guessed from plot)
sleeptab[as.logical((sleeptab$bwt < 2000) * (sleeptab$brwt > 1000)),]
# Man
sleeptab[as.logical((sleeptab$bwt < 4000) * (sleeptab$brwt > 4000)),]
# Asianelephant
sleeptab[as.logical((sleeptab$bwt > 4000) * (sleeptab$brwt > 5000)),]
# Africanelephant


# Exercise 2 ----
# 2.1
# Remove the extreme observations you saw in exercise 1
# Consider polynomial regression for non-transformed data from order 
# p = 1 to p = 5.
# Compute the pMSE for each polynomial
# Use a training data size n = floor(0.8*N)
# Plot sqrt(pMSE) vs p.

# Identify the outliers and remove them
outliers <- which(brwt>1000)
sleeptab_without_outliers <- sleeptab[-outliers, ]

# New total number of mammals
N <-  nrow(sleeptab_without_outliers)
set.seed(321) # For reproducibility

# Random sample of 80% of the total dataset used as trainging data
indeces <- sample.int(N, floor(0.8*N))
n <- N-length(indeces)
training <- sleeptab_without_outliers[indeces,]
testing <- sleeptab_without_outliers[-indeces,]

pMSE <- c()
for (p in 1:5) {
  # Fit y_train using polynomial of order p
  poly_model <- lm(training$brwt~poly(training$bwt, degree = p, raw = TRUE))
  # Obtain parameter estimates
  poly_coeff <- poly_model$coefficients
  # Make predictions based x_test covariates, as in eq2:
  # y_predicted(p) = b0 + b1*x_test + ... + bp*(x_test)^p
  # Use a loop to avoid writing it all by hand for each p
  predicted_brwt_poly <- poly_coeff[1]
  for (i in 1:p) {
    predicted_brwt_poly <- predicted_brwt_poly+poly_coeff[i+1]*(testing$bwt)^i
  }
  plot(testing$bwt, predicted_brwt_poly, main = p)
  # Compute and store pMSE(p)
  pMSE[p] <- 1/(N-n) * sum( (testing$brwt-predicted_brwt_poly)^2)
}

# Create dataframe for ggplot, then plot it and save it
pMSE_for_plot <- data.frame("sqrt(pMSE)" = sqrt(pMSE), "degree" = 1:5)
ggplot(pMSE_for_plot, aes(x = degree, y = sqrt(pMSE)))+
  geom_point()
if (savePlots) ggsave("figures/Ex2_1_1.png", 
                      width = 1770, height = 1000, units = "px")
if (closePlots) graphics.off()
training

# 2.2
# We wish to identify the best model in terms of sqrt(pMSE), while remembering 
# Occam's principle. 
# Give your reasoning regarding which model is sufficiently good.

# See comments in the paper

# 2.3
# Plots like those in 2.1 may change depending on randomly sampling different 
# units when creating the teseting and training dataset.
# Therefore try the following:
# 1. Put the whole procedure you codede in a loop, where you execute the model 
#    selection 500 times, each time sampling different testing and training data
# 2. Collect into a matrix all pMSE values for each degree and forn each of the 
#    500 runs.
# 3. For each value of p, plot the median of the sqrt(pMSE) values across the 
#    500, then for each p add on the same plot the lines connecting the 0.1 and 
#    0.9 quantiles.
# By looking at the plot, does your conclusion change about the best model?

# Set the size of the training dataset
N <-  nrow(sleeptab_without_outliers)
trainingSize <- floor(0.8*N)
n <- N-trainingSize
pMSE <- matrix(nrow = 500, ncol = 5) # Empty matrix with the correct size

set.seed(321) # For reproducibility
for (B in 1:500) {
  # Randomly sample indeces and create the trainging and testing dataset
  indeces <- sample.int(N, trainingSize)
  training <- sleeptab_without_outliers[indeces,]
  testing <- sleeptab_without_outliers[-indeces,]
  
  for (p in 1:5) {
    # Fit y_train using polynomial of order p
    poly_model <- lm(training$brwt~poly(training$bwt, degree = p, raw = TRUE))
    # Obtain parameter estimates
    poly_coeff <- poly_model$coefficients
    # Use the loop from above
    predicted_brwt_poly <- poly_coeff[1]
    for (i in 1:p) {
      predicted_brwt_poly <- predicted_brwt_poly+poly_coeff[i+1]*(testing$bwt)^i
    }
    # Compute and store pMSE(p)
    pMSE[B, p] <- sqrt(1/(N-n) * sum( (testing$brwt-predicted_brwt_poly)^2))
  }
}

# Create an empty data frame for use by ggplot
pMSE_intervals <- data.frame("Low"=0, "High"=0, "Median"=0) 
# Insert the quantile values and the median value for each degree p
for (i in 1:ncol(pMSE)) {
  pMSE_intervals[i, 1:2] <- quantile(pMSE[, i], c(0.1, 0.9))
  pMSE_intervals[i, "Median"] <- median(pMSE[, i])
}
# Add the degree p to the dataframe, makes it easier to plot later
pMSE_intervals["Degree"] <- 1:5

# PLot it and save it
ggplot(pMSE_intervals, aes(x=Degree))+
  geom_point(aes(y=Median))+
  geom_line(aes(y = Low))+
  geom_line(aes(y = High))+
  geom_hline(yintercept = min(pMSE_intervals$Median), col="red")+
  labs(title = "Exercise 2.3: Median pMSE, 500 repeats",
       subtitle = 
         "With 0.1 and 0.9 quantiles as lines and the lowest median in red",
       y = "sqrt(pMSE)")
if (savePlots) ggsave("figures/Ex2_3.png", 
                      width = 1770, height = 1000, units = "px")
if (closePlots) graphics.off()

# We need to detach it since we will load in and attach the dataset again in the
# next task.
detach(sleeptab)


# 2.4 ----
# CONSIDER THE FULL DATASET, RELOADING IT AGAIN HERE
sleeptab <- read.table('sleeptab.dat',header=TRUE) # loads the dataset
attach(sleeptab) # this way you can access the variables by name

# 2.4.a
# Find the optimal lambda for the Box-Cox transformation applied to the model 
# with response brwt and covariate bwt, report the loglikelihood vs labmda, and 
# then show the plot of the transformed response v.s. bwt. 
# This is supposed to look quite unsatisfactory.

# Create a simple linear model
simple_boxcox_model <- lm(brwt~bwt)
# Calculate the lambda and output a plot for log-likelihood v.s. lamda
if (savePlots) png("figures/Ex2_4_boxcox.png", 
                   width = 15, height = 8, units = "cm", res = 300)
boxcox_out <- boxcox(simple_boxcox_model)
if (closePlots) graphics.off()

# Find the maximum lambda, and save it in a variable
idmax <- which(boxcox_out$y==max(boxcox_out$y))
lambda <- boxcox_out$x[idmax]

# Since 0 is included in the 95% confidence interval, a log-transformation of 
# the response is used:
ggplot(sleeptab, aes(x = bwt, y = log(sleeptab$brwt)))+
  geom_point()+
  labs(title = "Exercise 2.4.a: Log-transformed response",
       y = "log(brwt)")
if (savePlots) ggsave("figures/Ex2_4_a.png",
                      width = 1770, height = 1000, units = "px")
if (closePlots) graphics.off()


# 2.4.b
# However, we know from previous exercises that log(bwt) could be a better 
# covariate, as the bwt spans different orders of magnitudes. 
# Do the same as in a, starting from a model with the untransformed brwt but 
# this time with covariate log(bwt). 
# Did you get something familiar?

# Create the model, run boxcox on it and save the plot
logbwt_boxcox_model <- lm(brwt~log(bwt))
if (savePlots) png("figures/Ex2_4_boxcox_logbwt.png", 
                   width = 15, height = 8, units = "cm", res = 300)
logbwt_boxcox_out <- boxcox(logbwt_boxcox_model)
if (closePlots) graphics.off()

# Identify the maximum lambda and save it
logbwt_idmax <- which(logbwt_boxcox_out$y==max(logbwt_boxcox_out$y))
logbwt_lambda <- logbwt_boxcox_out$x[logbwt_idmax]

# Since 0 is included in the 95% confidence interval, a log-transformation of 
# the response is used:
ggplot(sleeptab, aes(x = log(sleeptab$bwt), y = log(sleeptab$brwt)))+
  geom_point()+
  labs(title = "Exercise 2.4.b: Log-transformed response and covariate",
       y = "log(brwt)",
       x = "log(bwt)")
if (savePlots) ggsave("figures/Ex2_4_b.png", width = 1770, height = 1000, units = "px")
if (closePlots) graphics.off()


# 2.4.c
# Now use something different, we want to use lifespan (or a transformation 
# thereof) as covariate and brwt (or a transformation thereof) as response. 
# Produce a sensible model by building on what you learned in a-b.

# Create the model, run boxcox on it and save the plot
own_boxcox_model <- lm(brwt~lifespan)
if (savePlots) png("figures/Ex2_4_boxcox_own.png", 
                   width = 15, height = 8, units = "cm", res = 300)
own_boxcox_out <- boxcox(own_boxcox_model)
if (closePlots) graphics.off()

# Identify the maximum lambda and save it
own_idmax <- which(own_boxcox_out$y==max(own_boxcox_out$y))
own_lambda <- own_boxcox_out$x[own_idmax]

# Identify the 95% boundary for the lambda values:
top_lambda <- which(own_boxcox_out$y>quantile(own_boxcox_out$y, probs = 0.95))
# Identify the lambda corresponding to the lower limit
own_boxcox_out$x[top_lambda[1]]
# 0.02020202

# Since 0 is not included in the 95% confidence interval, a transformation of 
# the response is used:
lifespan_plot <- data.frame("lifespan" = lifespan, 
                            "response" = (brwt^own_lambda - 1)/own_lambda)
ggplot(lifespan_plot, aes(x = lifespan, y = response))+
  geom_point()+
  labs(title = "Lambda-transformed brwt",
       y = "(brwt^lambda - 1)/lambda")
if (savePlots) ggsave("figures/Ex2_4_c1.png", 
                      width = 1770, height = 1000, units = "px")
if (closePlots) graphics.off()

# Since the 95% confidence interval is very close to 0, a log-transformation of 
# the response is also used and compared to:
if (savePlots) png("figures/Ex2_4_c2.png")
plot(lifespan, log(brwt), main = "Log brwt, lifespan")
if (closePlots) graphics.off()

lifespan_plot_logbrwt <- data.frame("lifespan" = lifespan, 
                                    "log(brwt)" = log(brwt))
ggplot(lifespan_plot_logbrwt, aes(x = lifespan, y = log(brwt)))+
  geom_point()+
  labs(title = "Log brwt, lifespan")
if (savePlots) ggsave("figures/Ex2_4_c2.png", 
                      width = 1770, height = 1000, units = "px")
if (closePlots) graphics.off()


# If the covariate (lifespan) is log-transformed prior to the analysis, the 
# confidence interval includes zero, and therefore a log-transformation of the 
# response is also used.
own_boxcox_model_loglife <- lm(brwt~log(lifespan))
own_boxcox_out_loglife <- boxcox(own_boxcox_model_loglife)
if (savePlots) png("figures/Ex2_4_c3.png")
plot(log(lifespan), log(brwt), main = "Both log-transformed")
if (closePlots) graphics.off()

loglog_plot<- data.frame("log(lifespan)" = log(lifespan), 
                         "log(brwt)" = log(brwt))
ggplot(loglog_plot, aes(x = log(lifespan), y = log(brwt)))+
  geom_point()+
  labs(title = "Both log-transformed")
if (savePlots) ggsave("figures/Ex2_4_c3.png", 
                      width = 1770, height = 1000, units = "px")
if (closePlots) graphics.off()


# Others, just because it is interesting:
plot(lifespan, brwt, main = "Both untransformed")
plot(log(lifespan), brwt, main = "Log lifespan")
plot(log(lifespan), (brwt^own_lambda - 1)/own_lambda, 
     main = "Log lifespan, transform brwt")
plot(lifespan, log(brwt), main = "Log brwt")

# END
detach(sleeptab)