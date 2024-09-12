rm(list = ls())
setwd("~/Documents/RStudio/TMS150 - Stochastic Data Processing/AssignmentA2a")
#setwd("~/Documents/TMS150-Statistical-Data-Processing/AssignmentA2a")

library(ggplot2)


# Response variable y = brwt = brain weight
# Covariate x = bwt = body weight

# Exercise 1 ----
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
predicted_brwt <- matrix(nrow = 2000, ncol = 16) # Empty matrix with the correct size
for (B in 1:2000) {
  error <- rnorm(length(log_bwt), 0, sigma(ex1model)) # The correct number of error terms, new for each iteration
  #predicted_brwt[B, ] <- predict(ex1model, newdata = data.frame(bwt = newBWT)) #Default to assume that future observations have the same error variance as those used for fitting.
  predicted_brwt[B, ] <- coeffex1[1] + coeffex1[2]*log_bwt + error # Manually create the predicted values using the error term. Size of log_bwt = size of error -> elementwise operations. Gives an array which we place on row B.
}

confidenceIntervals <- data.frame("low"=0, "high"=0) # Create an empty data frame
for (i in 1:16) {
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

detach(sleeptab)

