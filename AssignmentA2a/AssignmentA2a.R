rm(list = ls())
setwd("~/Documents/RStudio/TMS150 - Stochastic Data Processing/AssignmentA2a")
#setwd("~/Documents/TMS150-Statistical-Data-Processing/AssignmentA2a")

library(ggplot2)


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

ex1model <- lm(log(brwt)~log(bwt))
coeffex1 <- ex1model$coefficients
set.seed(321)

log_bwt <- -5:10 # The equispaced values of log(bwt)
predicted_brwt <- matrix(nrow = 2000, ncol = length(log_bwt)) # Empty matrix with the correct size
for (B in 1:2000) {
  error <- rnorm(length(log_bwt), 0, sigma(ex1model)) # The correct number of error terms, new for each iteration
  #predicted_brwt[B, ] <- predict(ex1model, newdata = data.frame(bwt = newBWT)) #Default to assume that future observations have the same error variance as those used for fitting.
  predicted_brwt[B, ] <- coeffex1[1] + coeffex1[2]*log_bwt + error # Manually create the predicted values using the error term. Size of log_bwt = size of error -> elementwise operations. Gives an array which we place on row B.
}

confidenceIntervals <- data.frame("low"=0, "high"=0) # Create an empty data frame
for (i in 1:length(log_bwt)) {
  confidenceIntervals[i, ] <- quantile(predicted_brwt[,i], c(0.025, 0.975)) # Use the correct values here you dumdum
}
confidenceIntervals["logBWT"] <- log_bwt #Add so it is easier to plot

# GGPLOT BECAUSE
ggplot()+
  geom_point(data = sleeptab, aes(x=log(bwt), y=log(brwt)))+#, shape = 1
  geom_line(data = confidenceIntervals, aes(x = logBWT, y = low), linetype = "longdash")+
  geom_line(data = confidenceIntervals, aes(x = logBWT, y = high), linetype = "longdash")+
  labs(title = "Log-transformed variables",
       subtitle = "With 95% confidence interval (dashed lines)")

#TODO:
# [ ] Niceify plot
# [ ] Reformat code? Rename at least.
# [ ] Comment code
# [ ] Answer questions
# [ ] Write answers in LATEX


# 1.2 Same as above, but for nontransformed data and using bwt in 
# [0, 10, 20, ... 7000]
# What do we conclude in this case? 
# - Once more, here you find that some observations are outside the intervals. 
# - Are these outside by a considerable amount? 
# - Comment on your findings.

ex12model <- lm(brwt~bwt)
coeffex12 <- ex1model$coefficients
set.seed(321)

new_bwt <- seq(0, 7000, 10) # The equispaced values of bwt
predicted_brwt_nontrans <- matrix(nrow = 2000, ncol = length(new_bwt)) # Empty matrix with the correct size
for (B in 1:2000) {
  error <- rnorm(length(new_bwt), 0, sigma(ex12model)) # The correct number of error terms, new for each iteration
  #predicted_brwt[B, ] <- predict(ex1model, newdata = data.frame(bwt = newBWT)) #Default to assume that future observations have the same error variance as those used for fitting.
  predicted_brwt_nontrans[B, ] <- coeffex12[1] + coeffex12[2]*new_bwt + error # Manually create the predicted values using the error term. Size of log_bwt = size of error -> elementwise operations. Gives an array which we place on row B.
}

confidenceIntervals_nontrans <- data.frame("low"=0, "high"=0) # Create an empty data frame
for (i in 1:length(new_bwt)) {
  confidenceIntervals_nontrans[i, ] <- quantile(predicted_brwt_nontrans[,i], c(0.025, 0.975)) # Use the correct values here you dumdum
}
confidenceIntervals_nontrans["new_BWT"] <- new_bwt #Add so it is easier to plot

# GGPLOT BECAUSE
ggplot()+
  geom_point(data = sleeptab, aes(x=bwt, y=brwt))+#, shape = 1
  geom_line(data = confidenceIntervals_nontrans, aes(x = new_BWT, y = low))+
  geom_line(data = confidenceIntervals_nontrans, aes(x = new_BWT, y = high))+
  labs(title = "Non-transformed variables",
       subtitle = "With 95% confidence interval (dashed lines)")


detach(sleeptab)



