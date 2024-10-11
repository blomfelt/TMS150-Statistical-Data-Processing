# QUESTION 2
import random
import numpy as np
import matplotlib.pyplot as plt
import time 
start_time = time.time()

# Create h:
h = np.array([2**-i for i in range(10, 0, -1)])
number_steps = int(1/min(h))

# Generate all eta:s for the smallest level
eta_finest = np.zeros(number_steps)
random.seed("felix")
for e in range(number_steps):
    # Normal distribution, give it sigma not sigma^2
    eta_finest[e] = random.gauss(0, np.sqrt(min(h)))

# For the exact X, we use the small timesteps
timesteps_finest = np.array([n/number_steps for n in range(number_steps+1)])

#Define constants and create necessary empty arrays
W_finest = np.zeros(number_steps+1)
X_exact = np.zeros(number_steps+1)
mu = 2
sigma = 1

# Create the W for the finest level
for j in range(1, number_steps+1):
    W_finest[j] = W_finest[j-1] + eta_finest[j-1]
X_exact = np.exp((mu-(sigma**2)/2)*timesteps_finest + sigma*W_finest)
plt.plot(timesteps_finest, X_exact, label = "True X")

#Init loop
eta_previous = eta_finest
# Use a loop which uses the previous eta:s, halve the number of steps and 
# create the next granularity of eta and W.
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
    # Start at 0 since W(0) = 0, up to level size +1 since non-inclusive range
    for j in range(1, level_size+1): 
        W_level[j] = W_level[j-1] + eta_next[j-1]
    #plt.plot(timesteps_level, W_level)
    
    # Create empty array and set X(0) to the correct value
    X_level = np.zeros(level_size+1)
    X_level[0] = 1
    # Calculate the approximated X for this resolution
    for x in range(1, level_size+1):
        X_level[x] = (1+h[i]*mu)*X_level[x-1] + sigma*X_level[x-1]*(W_level[x] - W_level[x-1])

    # Add the approximation for this resolution to the plot
    plt.plot(timesteps_level, X_level, label = "i = %s" % (10-i))

    # Prepare for next loop
    eta_previous = eta_next

# Plot it and save it
plt.title("Question 2")
plt.xlabel("t")
plt.ylabel("X(t)")
plt.legend()
plt.savefig("figures/q2.png")


print("DONE in %.2f seconds" % (time.time() - start_time))
