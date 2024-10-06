# Import libraries
import random
import numpy as np
import matplotlib.pyplot as plt
import time

start_time = time.time()
# Question 6
# Let f and theta be as in question 1.
# In order to estimate the root mean squared error, use another Monte Carlo 
# estimate with N = 10, i.e. compute (TODO:)
# Observe that you need to use one more loop over N, in addition to the one 
# over M as you have done in Question 5. 
# Plot your results in a similar way as in the previous task. 
# Is the result expected?
# What happens if you change N?

M = np.array([2**i for i in range(1, 21)])
#M = np.array([2**i for i in range(1, 11)])
N = 1000

# Allocate empty vectors
hat_theta_MC = np.zeros([len(M), N])
hat_theta_HM = np.zeros([len(M), N])
rms_MC = np.zeros(N)
rms_HM = np.zeros(N)

# Set constants
sample_size = 1000
analytical_theta = 5/24 

# Set seed
random.seed(321)
for i in range(len(M)):
    # Do the above for MC and HM.
    # Save in array using theta_MC[index] = value?

    
    for j in range(N): #TODO: why N-1? 
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

