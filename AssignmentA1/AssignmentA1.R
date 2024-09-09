#setwd("~/Documents/RStudio/TMS150 - Stochastic Data Processing/AssignmentA1")
setwd("~/Documents/TMS150-Statistical-Data-Processing/AssignmentA1")

# To save all plots change to TRUE
savePlots <-  FALSE

# Response variable y = brwt = brain weight
# Covariate x = bwt = body weight

# Exercise 1 ----
sleeptab <- read.table('sleeptab.dat',header=TRUE) # loads the dataset
# the option header = TRUE tells R that the first row
# contains the variables names
attach(sleeptab) # this way you can access the variables by name
# Inspect the data:
head(sleeptab)

# 1.1
if (savePlots) png("figures/Ex1_1.png")
plot(bwt, brwt)
dev.off()

# 1.2
tooLarge <- which(brwt>1000)
Species_of_animal[tooLarge]
# "Africanelephant" "Asianelephant" "Man"
brwt[tooLarge]
# 5712 4603 1320

if (savePlots) png("figures/Ex1_2.png")
plot(bwt[-tooLarge], brwt[-tooLarge])
dev.off()
# Very linear for the smallest values, but less so for the largest.
# An increase in body weight does seem to increase the brain weight.

# 1.3 
# Linear fit by hand
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
plot(bwt, brwt)
abline(sleepModel)

cat("Manual calc: \n", b0, b1,
    "\nUsing ´lm´:\n", sleepModel$coefficients)
# We get the same result when doing it manually and using the function, 
# which is good! 
# BUT using the function seems exponentially easier the more variables you have


# 1.4 
# The value b1 represent the slope, so an increase of 1 kg body weight correlate
# with an increase of 0.97 g brwt. This means that an 100 kg increase in body
# weight would lead, in average, to a 96 grams increase of brain weight.

# Exercise 2 ----
# You may use the predict() function
# 2.1 
if (savePlots) png("figures/Ex2_1.png")
plot(sleepModel$residuals)
abline(h=0, col = "red")
dev.off()
sleepModel$residuals[abs(sleepModel$residuals)>500]
#         1         5        34 
# -810.0712 2050.3294 1169.0728 
# The same three as before

# 2.2
logBwt <- log(bwt)
logBrwt <- log(brwt)

if (savePlots) png("figures/Ex2_2.png")
plot(logBwt, logBrwt)
dev.off()

logSleepModel <- lm(logBrwt~logBwt)

if (savePlots) png("figures/Ex2_2.png")
plot(logBwt, logBrwt)
abline(logSleepModel)
dev.off()

# 2.3
# Estimate the value of brain weight when the body weight is 1000 kg using both
# the original model and the one from the transformed variables. Don't forget
# to exponentiate the result. Also give the difference between them.
# Comment on whether you think this is substantial. 

#First model
predict(sleepModel, newdata = data.frame(bwt=1000))
# 1057.501 
# Transformed model
exp(predict(logSleepModel, newdata = data.frame(logBwt = log(1000))))
# 1521.195 

# 1521.195 - 1057.501 = 463.694
# 463.694 / 1057.501 = 0.4384809

# COMMENT TODO


# Estimation for later
x0 <- seq(min(bwt), max(bwt), length = 100) 
E0 <- predict(sleepModel, newdata = data.frame(bwt = x0))

# now produce plots of the estimated m2, by predicting from a grid of x0 values defined as below
# estimate E(y) according to x0
E0 <- predict(sleepModel, newdata = data.frame(bwt = x0))  # estimated expected distance using model m2 based on speed set at x0 
# add the predictions we have just obtained to the plot with data
lines(x0, E0, col = "red")  # add regression curve (colour: red)
