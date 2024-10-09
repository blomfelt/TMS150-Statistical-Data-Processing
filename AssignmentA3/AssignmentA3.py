# Import libraries
import random
import numpy as np
import matplotlib.pyplot as plt
import time

# Consider for the questions of this project the function f: [0, 1] -> [0, 1]
# f(x) = 0.5 * (x^5 +x^3)
# and let
# theta = \int{0}{1} f(x) dx



## Question 1
# Compute theta analytically.
# Write then a program that estimates for fixed sample size m an estimation of 
# theta with crude Monte Carlo

analytical_theta = 5/24 # ≈ 0.20833

# Set sample size
m = 1000

# Set initial theta
theta = 0

# Set seed
random.seed(321) 

for k in range(m-1):
    # Generate u ~ U(0, 1)
    u = random.uniform(0, 1)
    # Calculate new value and add to old
    theta += 0.5*(u**5 + u**3)

print("\nQuestion 1\n")
print("Analytical theta: " + str(analytical_theta))
print("theta/m = " + str(theta/m) + "\n\n")
# 0.20949, correct is 0.20833, 0.56% off



# Question 2
# Let f and theta be as in question 1.
# Estimate for fixed sample size m an estimation of theta with hit-or-miss MC.

# Set sample size
m = 1000

# Set initial count
count = 0

for k in range(m-1):
    # Generate u1, u2 ~ U(0, 1)
    u1 = random.uniform(0, 1)
    u2 = random.uniform(0, 1)
    if u2<= 0.5*(u1**5 + u1**3):
        count += 1

print("\nQuestion 2\n")
print("count/m = " + str(count/m) + "\n\n")
# 0.191, correct is 0.20833, 8.3% off


# Question 3
# Let f and theta be as in question 1.
# Compute analytically the variance of both estimators for the given function f
# and the root mean squared error. 
# Write out in detail your analytical calculations.

# X = crude MC
# Y = hit-or-miss MC 

# Calculated constants (calculated value divided by sample size)
variance_mc_analytical = (3131/44352)/1000
variance_hm_analytical = (95/576)/1000
rmse_mc_analytical = np.sqrt(variance_mc_analytical)
rmse_hm_analytical = np.sqrt(variance_hm_analytical)

print("\nQuestion 3\n")
print("Variance:")
print("MC = " + str(variance_mc_analytical))
print("HM = " + str(variance_hm_analytical))
print("\nRMSE:")
print("MC = " + str(rmse_mc_analytical))
print("HM = " + str(rmse_hm_analytical))



# Question 4
# Let f and theta be as in question 1.
# Estimate numerically with MC the root mean squared error of both estimators
# for different sample numbers N, and compare it to your analytical results 
# from the previous task. 

# Set constants
sample_size_M = 1000
N = np.array([i for i in range(1, 100)])
known_theta = 5/24

# Create empty arrays
rmse_mc_computed = np.zeros(len(N))
rmse_hm_computed = np.zeros(len(N))

# Set seed
random.seed(321) 
for n in range(len(N)):
    # Initiate empty vectors of the correct size
    estimation_mc = np.zeros(N[n]) 
    estimation_hm = np.zeros(N[n]) 
    
    for i in range(N[n]):
        # Calculate this the same way as before
        #Crude
        theta = 0 # Set intial count
        for k in range(sample_size_M-1):
            # Generate u ~ U(0, 1)
            u = random.uniform(0, 1)
            # Calculate new value and add to old
            theta += 0.5*(u**5 + u**3)

        #Hit-or-miss
        # Set initial count
        count = 0

        for k in range(sample_size_M-1):
            # Generate u1, u2 ~ U(0, 1)
            u1 = random.uniform(0, 1)
            u2 = random.uniform(0, 1)
            if u2<= 0.5*(u1**5 + u1**3):
                count += 1
        # Return theta/M for each i
        estimation_mc[i] = theta/sample_size_M
        estimation_hm[i] = count/sample_size_M
    # Use given formula to calculate the estimated RMSE for each sample szie
    rmse_mc_computed[n] = np.sqrt(1/N[n] * sum((estimation_mc - known_theta)**2))
    rmse_hm_computed[n] = np.sqrt(1/N[n] * sum((estimation_hm - known_theta)**2))

# Debug printing
#print("\nQuestion 4\n")
#print("\nEstimation MC:")
#print(estimation_mc)
#print("\nEstimation MC - theta:")
#print(estimation_mc - known_theta)
#print("\nRMSE MC:")
#print(rmse_mc_computed)
#print("\nRMSE HM:")
#print(rmse_hm_computed)


# Plot both lines in the same plot
plt.figure()
plt.plot(rmse_mc_computed, label = "Crude MC")
plt.plot(rmse_hm_computed, label = "Hit-or-miss MC")
# Format plot
plt.ylabel("RMSE")
plt.xlabel("Sample size N")
plt.legend()
plt.savefig("figures/q4_both.png")



# Question 5
# Let f and theta be as in question 1.
# Write a program that:
# 1. Computes the Monte-Carlo estimates \hat{theta}_i^MC \hat{theta}_i^HM based
#    on both estimators for a sequence of samples M = (2^i, i = 1, ..., 20)
# 2. Compute the errors [\hat{theta}_i^MC - theta] and 
#    [\hat{theta}_i^HM - theta] using your analytical result theta.
# 3. Plot your results in a loglog plot
# 4. Add your theoretical reference slope M^-1/2.
# 5. What do you observe?

# Range not inclusive when give start, therefore to 21
M = np.array([2**i for i in range(1, 21)]) 

# Allocate empty vectors
hat_theta_MC = np.zeros(20)
hat_theta_HM = np.zeros(20)

# Set constants
known_theta = 5/24

# Set seed
random.seed(321) 
for i in range(len(M)):
    # Do the same as above for MC and HM.
    
    # CRUDE MC
    # Set initial theta
    theta = 0

    for k in range(M[i]-1):
        # Generate u ~ U(0, 1)
        u = random.uniform(0, 1)
        # Calculate new value and add to old
        theta += 0.5*(u**5 + u**3)
    hat_theta_MC[i] = theta/M[i]

    # HIT-OR-MISS
    count = 0

    for k in range(M[i]-1):
        # Generate u1, u2 ~ U(0, 1)
        u1 = random.uniform(0, 1)
        u2 = random.uniform(0, 1)
        if u2<= 0.5*(u1**5 + u1**3):
            count += 1
    hat_theta_HM[i] = count/M[i]

print("\nQuestion 5\n")
print("Hat_theta_MC")
print(hat_theta_MC)
print("\nHat_theta_HM")
print(hat_theta_HM)

error_MC = abs(hat_theta_MC - known_theta)
error_HM = abs(hat_theta_HM - known_theta)

# Plot al three lines
plt.figure()
plt.loglog(M, error_MC, label = "Crude MC")
plt.loglog(M, error_HM, label = "Hit-or-Miss")
plt.loglog(M, 1/np.sqrt(M), label = "1/sqrt(M)")

# Format plot
plt.title("Error for MC v.s. HM")
plt.ylabel("Error")
plt.xlabel("Sample size M")
plt.legend() 

plt.savefig("figures/q5.png")



# Question 6
# Let f and theta be as in question 1.
# In order to estimate the root mean squared error, use another Monte Carlo 
# estimate with N = 10, i.e. compute
# Observe that you need to use one more loop over N, in addition to the one 
# over M as you have done in Question 5. 
# Plot your results in a similar way as in the previous task. 
# Is the result expected?
# What happens if you change N?
start_time = time.time()

# Set constants
M = np.array([2**i for i in range(1, 21)])
N = 10
sample_size = 1000
analytical_theta = 5/24 

# Allocate empty vectors
hat_theta_MC = np.zeros([len(M), N])
hat_theta_HM = np.zeros([len(M), N])
rms_MC = np.zeros(N)
rms_HM = np.zeros(N)

# Set seed
random.seed(321)
for i in range(len(M)):
    # Do the same as above for MC and HM.

    for j in range(N): 
        # CRUDE MC
        # Set initial theta
        theta = 0
        for k in range(M[i]-1):
            # Generate u ~ U(0, 1)
            u = random.uniform(0, 1)
            # Calculate new value and add to old
            theta += 0.5*(u**5 + u**3)
        hat_theta_MC[i, j] = theta/M[i]
    
        # HIT-OR-MISS
        count = 0
    
        for k in range(M[i]-1):
            # Generate u1, u2 ~ U(0, 1)
            u1 = random.uniform(0, 1)
            u2 = random.uniform(0, 1)
            if u2<= 0.5*(u1**5 + u1**3):
                count += 1
        hat_theta_HM[i, j] = count/M[i]
        # Debug printing
        #print("Done with N = " + str(j) + ", M = " + str(M[i]))

#Sum each row, each difference between computed and analytical, using M samples
sum_rows_mc = np.sum(hat_theta_MC - analytical_theta, axis = 1)
# Calculate the RMSE for each M
for_each_M_mc = np.sqrt((abs(sum_rows_mc)**2)/N)

#Do the same for HM
sum_rows_hm = np.sum(hat_theta_HM - analytical_theta, axis = 1)
for_each_M_hm = np.sqrt((abs(sum_rows_hm)**2)/N)

# Plot all three lines
plt.figure()
plt.loglog(M, for_each_M_mc, label = "Crude MC")
plt.loglog(M, for_each_M_hm, label = "Hit-or-miss MC")
plt.loglog(M, 1/np.sqrt(M), label = "1/sqrt(M)")
# Format plot
plt.title("RMSE using N = " + str(N))
plt.xlabel("Sample size M")
plt.ylabel("RMSE")
plt.legend()
plt.savefig("figures/q6_" + str(N) + ".png")

print("\nQuestion 6\n")
print("--- %s seconds ---" % (time.time() - start_time))
