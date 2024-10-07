# QUESTION 2
import random
import numpy as np
import matplotlib.pyplot as plt
import time 
start_time = time.time()
# Create h:
h = np.array([2**-i for i in range(10, 0, -1)])
number_steps = int(1/min(h))


# Set constants
M = 1000
last_X = np.zeros([M, len(h)])
random.seed("felix")
# Use a loop which uses the previous eta:s, halve the number of steps and 
# create the next granularity of eta and W.
for m in range(M):
    # Generate all eta:s for the smallest level
    eta_finest = np.zeros(number_steps)
    for e in range(number_steps):
        eta_finest[e] = random.gauss(0, min(h))
    #Init loop
    eta_previous = eta_finest

    for i in range(len(h)):
        #print("\ni = " + str(i))
        # Sum every two eta:s to form the next level
        level_size = int(len(eta_previous)/2)
        eta_next = np.zeros(level_size)
        W_level = np.zeros(level_size+1)
        timesteps_level = np.array([n/level_size for n in range(level_size+1)])
        for e in range(level_size):
            eta_next[e] = eta_previous[e*2] + eta_previous[e*2+1]
        
        # Create the next W using the previous and the corresponding eta
        for j in range(1, level_size+1): #Level size +1 since non-inclusive range
            W_level[j] = W_level[j-1] + eta_next[j-1]
        #plt.plot(timesteps_level, W_level)
        
        # NEW
        mu = 2
        sigma = 1
        
        if i == 0: # For only the first loop, finest level
            X_exact = np.exp((mu-(sigma**2)/2)*timesteps_level + sigma*W_level)
            #plt.plot(timesteps_level, X_exact)
    
        X_level = np.zeros(level_size+1)
        X_level[0] = 1
        for x in range(1, level_size+1):
            X_level[x] = (1+h[i]*mu)*X_level[x-1] + sigma*X_level[x-1]*(W_level[x] - W_level[x-1])
        #plt.plot(timesteps_level, X_level)
        last_X[m, i] = X_level[-1]

        eta_previous = eta_next
    
strong_errors = np.sqrt(sum((last_X - X_exact[-1])**2)/M)

# Plot and save
plt.plot(h, strong_errors, label = "Strong Error")
plt.plot(h, np.sqrt(h), label = "sqrt(h)")
plt.xlabel("h")
plt.ylabel("Error")
plt.legend()
#TODO: Add title
plt.title("Question 3")
plt.savefig("figures/q3.png")
#print(last_X-X_exact[-1])    

#print("X:")
#print(last_X)

#print("\nTheta")
#print(last_theta)

#print(last_X)






print("DONE in %.2f seconds" % (time.time() - start_time))
