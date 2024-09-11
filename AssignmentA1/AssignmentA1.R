rm(list=ls())
setwd("~/Documents/RStudio/TMS150 - Stochastic Data Processing/AssignmentA1")
#setwd("~/Documents/TMS150-Statistical-Data-Processing/AssignmentA1")

# To save all plots change to TRUE
savePlots <-  FALSE
closePlots <- FALSE

if(savePlots) closePlots <- TRUE

# Response variable y = brwt = brain weight
# Covariate x = bwt = body weight

# Exercise 1 ----
sleeptab <- read.table('sleeptab.dat',header=TRUE) # loads the dataset
# the option header = TRUE tells R that the first row
# contains the variables names
attach(sleeptab) # this way you can access the variables by name
# Inspect the data and show the structure of it:
head(sleeptab)

# 1.1
# Plot response v.s.covariate
if (savePlots) png("figures/Ex1_1.png")
plot(bwt, brwt, main = "Exercise 1.1")
if (closePlots) if (closePlots) graphics.off()

# Identify which species the three outliers belong to
tooLarge <- which(brwt>1000) # Get indexes of the extreme values
  # 1000 is guessed from the plot
Species_of_animal[tooLarge]
# "Africanelephant" "Asianelephant" "Man"
brwt[tooLarge] # Brain weight in grams:
# 5712 4603 1320
# TODO: Check, elaborate?

# 1.2
# Plot reponse without the outliers
if (savePlots) png("figures/Ex1_2.png")
plot(bwt[-tooLarge], brwt[-tooLarge], main = "Exercise 1.2") #Exclude the indexes of the extreme values
if (closePlots) if (closePlots) graphics.off()
# Very linear for the smallest values, but less so for the largest.
# An increase in body weight does seem to increase the brain weight.
# TODO: Check, elaborate?

# 1.3 
# Compute a linear fit by hand
# x = bwt
# y = brwt

# slope b1, formula from p 5 in recaplinear.pdf
b1 = sum((bwt-mean(bwt))*brwt) / sum((bwt-mean(bwt))^2)
#    sum( xi - mean(x)  * yi ) / sum((xi -mean( x ))^2)

# intercept b0
b0 = mean(brwt) - b1*mean(bwt)
#    mean( y  ) - b1*mean( x )

# Linear fit by 'lm'
sleepModel <- lm(brwt~bwt)
summary(sleepModel)
if (savePlots) png("figures/Ex1_3.png")
plot(bwt, brwt, main = "Exercise 1.3")
abline(sleepModel)
if (closePlots) graphics.off()

cat("Exercise 1.3:",
    "\nManual calc: \n", b0, b1,
    "\nUsing ´lm´:\n", sleepModel$coefficients)
# We get the same result when doing it manually and using the function, 
# which is good! 
# BUT using the function seems exponentially easier the more variables you have
# TODO: Check, elaborate?

# 1.4 
# Interpret the value b1. What can you say about it? And what does it help you 
# conclude about the increase in brain weight for a 100 kg increase in bwt?

# The value b1 represent the slope, so an increase of 1 kg body weight correlate
# with an increase of 0.97 g brwt. This means that an 100 kg increase in body
# weight would lead, in average, to a 96 grams increase of brain weight.
# TODO: Check, elaborate?

# Exercise 2 ----
# 2.1 
# Plot the residuals. Observe any extreme observation? Extreme by how much?
if (savePlots) png("figures/Ex2_1.png")
plot(sleepModel$residuals, main = "Exercise 2.1")
abline(h=0, col = "red")
if (closePlots) graphics.off()
sleepModel$residuals[abs(sleepModel$residuals)>500]
#         1         5        34 
# -810.0712 2050.3294 1169.0728 
# The same three as before
# TODO: Check, elaborate?
# [ ] Do you observe any extreme observation? 
# [ ] Extreme by how much?

# 2.2
# Log-transform both variables, and plot them. Perform regression using the
# transformed variables, then plot it.
logBwt <- log(bwt)   # Feels unnecessary to do this,
logBrwt <- log(brwt) # but the code was easier to read this way

plot(logBwt, logBrwt, main = "Exercise 2.2")
if (closePlots) graphics.off()

logSleepModel <- lm(logBrwt~logBwt)

if (savePlots) png("figures/Ex2_2.png")
plot(logBwt, logBrwt, main = "Exercise 2.2")
abline(logSleepModel)
if (closePlots) graphics.off()
# TODO: Check, elaborate?
# [ ] Better, no? -> Better, yes!

# 2.3
# Estimate the value of brain weight when the body weight is 1000 kg using both
# the original model and the one from the transformed variables. Don't forget
# to exponentiate the result. Also give the difference between them.
# Comment on whether you think this is substantial. 

# First model
predict(sleepModel, newdata = data.frame(bwt=1000))
# 1057.501 
# Transformed model
exp(predict(logSleepModel, newdata = data.frame(logBwt = log(1000))))
# 1521.195 

# 1521.195 - 1057.501 = 463.694
# 463.694 / 1057.501 = 0.4384809

# TODO: Check, elaborate?


# Exercise 3 ----
# 3.1 
# Calculate the value of s "by hand", check using confint().
# Formula:
#           sum( (y(i) - estimate(y))^2 )
# s = sqrt( ------------------------- )
#                   (n - 2)

n <- length(brwt)

# Use the body weight (bwt) to predict new response and calculate the residual
residualsManual <- brwt-predict(sleepModel, newdata = data.frame(bwt))
# Use the formula for s, see above, to calculate it
s <- sqrt(sum(residualsManual^2)/(n - 2))
# 334.7198
# TODO: elaborate? "The value of s is 334.7198."??

# 3.2 
# Calculate "by hand" the confidence interval of b0 and b1 using a confidence 
# level of 0.9.

# The confidence limit 1-alpha = 0.9 gives alpha = 0.1, which means that 
# alpha/2 = 0.05 and 1-alpha/2 = 0.95
# This gives 
n <-  length(bwt)
t_alpha <-  qt(c(0.05, 0.95), n-2)
b0 <- sleepModel$coefficients[1]
b1 <- sleepModel$coefficients[2]

Interval_b0 <- b0 + t_alpha*s*sqrt(1/n + mean(bwt)^2 / sum((bwt-mean(bwt))^2))
Interval_b1 <- b1 + t_alpha*s / sqrt(sum((bwt-mean(bwt))^2))

# TODO: Choose one printer:
confint(sleepModel, level = 0.9)
#cat(Interval_b0, "\n", Interval_b1)
cat("\n\nExercise 3.2:\n")
cat("\t\t", "5%\t\t95%", 
    "\n(Intercept)\t", Interval_b0[1], "\t", Interval_b0[2], 
    "\nbwt\t\t", Interval_b1[1], "\t", Interval_b1[2], sep = '')

cat("\n\nExercise 3.2:\n")
cat(format(c(
  "", "5%", "95%", 
  "b0, manual", signif(Interval_b0, 7), 
  "b0, code", signif(confint(sleepModel, level = 0.90)[1,], 7), 
  "b1, manual", signif(Interval_b1, 7), 
  "b1, code", signif(confint(sleepModel, level = 0.90)[2,], 7)), 
  justify = 'right'), fill = 40)

# TODO: 
# [x] Calculate the confidence intervals by hand, with confidence limit 0.9.
# [x] You may not use `confint`, but may check your results with this.
# [ ] Add your concrete interpretation of such intervals.


# Exercise 4 ----

# Denote the already obtained least squares estimates with b0_old and b1_old.
# Lets pretend these are the true parameter values.

# Write a for-loop to produce 2000 sets of parameter estimates and confidence
# intervals.

# Plug the b0_old and b1_old into the linear model and produce simulated
# observations brwt_new(i) = b0_old + b1_old*bwt(i) + e_new(i), 
# for i = 1, ..., 62 where bwt(i) is the same values as the original datasets.

# Use s = 334.7198 (from Ex 3) with rnorm to produce e_new(i) ~ N(0, s^2)

# Fit the simulated dataset, D_1(bwt(i), brwt_new(i)), i=1...n, and denote the
# parameter estimates b0_hat(1) and b1_hat(1)

# Repeat the procedure to create new observations brwt_new(i) and e_new(i) using
# rnorm and call the new dataset D_2(bwt(i), brwt_new(i)), i=1...n. Fit D_2 via
# linear regression and denote the parameter estimates b0_hat(2) and b1_hat(2).

# Repeat until you have 2000 sets of estimates. Construct the confidence
# intervals (1-alpha=0.9) from each set of estimates, so that you have 2000
# intervals for b0_old and b1_old respectively.

# Compute the proportion of intervals that include the original value b0_old and
# b1_old respectively, and show that both proportions are very close to 1-alpha, as
# expected from the theory.

numEstimates <- 2000
numObservations <- 62

# From previous exercises
b0_old <- sleepModel$coefficients[1]
b1_old <- sleepModel$coefficients[2]
s <- sigma(sleepModel)

b0_new <- c()
b1_new <- c()
confint_b0 <- data.frame("low"=0, "high"=0)
confint_b1 <- data.frame("low"=0, "high"=0)

for (N in 1:numEstimates) {
  # Create 62 error terms:
  error <- rnorm(numObservations, mean = 0, sd = s^2) 
  # [ ] Should s above be s^2?
  
  # Predict new brainweights using the existing body weights and the error terms
  brwt_new <- b0_old + b1_old*bwt + error
  newmodel <- lm(brwt_new ~ bwt)
  
  # Save the new parameters
  b0_new[N] <- newmodel$coefficients[1] # Save them but just in case?
  b1_new[N] <- newmodel$coefficients[2]
  
  # Calculate the confidence intervals
  new_confint <- confint(newmodel, level = 0.9)
  confint_b0[N,] <- new_confint[1, ] # Add to row N in the matrix
  confint_b1[N,] <- new_confint[2, ]
}
# Count how many intervals includes the "true" value, i.e. count the ones where 
# the statements are both true and sum to 2.
includes_b0 <- sum((b0_old > confint_b0$low) + (b0_old < confint_b0$high)==2)
includes_b1 <- sum((b1_old > confint_b1$low) + (b1_old < confint_b1$high)==2)
cat("\nExercise 4:")
cat("\nThe proportion of intervals which include b0_old is", includes_b0/N)
cat("\nThe proportion of intervals which include b1_old is", includes_b1/N)


detach(sleeptab)


