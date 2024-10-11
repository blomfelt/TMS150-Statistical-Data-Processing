# QUESTION 3
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
mu = 2
sigma = 1
diff_X = np.zeros([M, len(h)])
random.seed("felix")

for m in range(M):
    # Generate all eta:s for the smallest level
    eta_finest = np.zeros(number_steps)
    for e in range(number_steps):
        # Normal distribution, give it sigma not sigma^2 
        eta_finest[e] = random.gauss(0, np.sqrt(min(h)))
    
    # For the exact X, we use the small timesteps
    timesteps_finest = np.array([n/number_steps for n in range(number_steps)])
    #Define constants and create necessary empty arrays for this resolution
    W_finest = np.zeros(number_steps+1)
    X_exact = np.zeros(number_steps+1)

    # Create the W for the finest level
    for j in range(1, number_steps+1):
        W_finest[j] = W_finest[j-1] + eta_finest[j-1]
    X_exact = np.exp((mu-(sigma**2)/2) + sigma*W_finest)

    #Init loop
    eta_previous = eta_finest

    for i in range(len(h)):
        level_size = int(len(eta_previous)/2)
        # Create empty arrays of the correct size for this resolution
        eta_next = np.zeros(level_size)
        W_level = np.zeros(level_size+1)
        # Create the correct timesteps
        timesteps_level = np.array([n/level_size for n in range(level_size+1)])
        # Sum every two eta:s to form the next level
        for e in range(level_size):
            eta_next[e] = eta_previous[e*2] + eta_previous[e*2+1]
        
        # Create the next W using the previous and the corresponding eta
        for j in range(1, level_size+1): #Level size +1 since non-inclusive range
            W_level[j] = W_level[j-1] + eta_next[j-1]

        # Create empty array and set X(0) to the correct value
        X_level = np.zeros(level_size+1)
        X_level[0] = 1
        # Calculate the approximated X for this resolution
        for x in range(1, level_size+1):
            X_level[x] = (1+h[i]*mu)*X_level[x-1] + sigma*X_level[x-1]*(W_level[x] - W_level[x-1])

        # Calculate difference between "true" value and approximation
        diff_X[m, i] = X_exact[-1] - X_level[-1]

        # Prepare for next loop
        eta_previous = eta_next

# Calculate the strong error
strong_errors = np.sqrt(sum((diff_X)**2))/M

# Plot and save
plt.loglog(h, strong_errors, 'o', label = "Strong Error")
plt.loglog(h, np.sqrt(h), label = "sqrt(h)")
plt.xlabel("h")
plt.ylabel("Error")
plt.legend()
plt.title("Question 3")
plt.savefig("figures/q3.png")


print("DONE in %.2f seconds" % (time.time() - start_time))
