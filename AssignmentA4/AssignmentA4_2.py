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

# Generate all levels of the grid, starting from the finest
W = {}
timesteps = {}
# Manually create the first/finest level
W[0] = np.concatenate(([0], np.cumsum(eta_finest)))
timesteps[0] = np.array([n*h[0] for n in range(int(1/h[0]+1))])

# Init loop
eta_finer = eta_finest
# Loop over all other levels
for e in range(1, len(h)):
    eta_coarser = eta_finer[::2] + eta_finer[1::2]
    W[e] = np.append([0], np.cumsum(eta_coarser))
    # Create the correct timesteps while we loop over, but note that these are 
    # 1 loop/step after, i.e. when W have 513 elements, timesteps have 1025 
    timesteps[e] = np.array([n*h[e] for n in range(int(1/h[e]+1))])
    eta_finer = eta_coarser
#TODO: print(W)

#Define constants and create necessary empty arrays
X_exact = np.zeros(number_steps+1)
mu = 2
sigma = 1

# Create the X_exact using the finest level W
X_exact = np.exp((mu-(sigma**2)/2)*timesteps[0] + sigma*W[0])
# Plot it
plt.plot(timesteps[0], X_exact, label = "True X")

# Create the approximation X for each level
X_all = {} 
for i in range(len(W)):
    h_level = h[i]
    level_size = int(1/h[i]+1)
    # Create empty X of correct sixe for this level
    X_level = np.zeros(level_size)
    # Set initial X[0] = 1
    X_level[0] = 1
    # Extract W for this level
    W_level = W[i]
    for n in range(1, level_size):
        X_level[n] = (1 + h_level * mu) * X_level[n-1] + sigma * X_level[n-1] * (W_level[n] - W_level[n-1])
    X_all[i] = X_level
    #print(X_level)
#print(X_all)

# Plot all the others:
for i in range(len(W)):
    plt.plot(timesteps[i], X_all[i], label = "i = %s" % (10 - i))
    # TODO: Change above

# Format plot and save it
plt.title("Question 2")
plt.xlabel("t")
plt.ylabel("X(t)")
plt.legend()
plt.savefig("figures/q2.png")

exit()


# OLD 
#Init loop
#eta_previous = eta_finest
# Use a loop which uses the previous eta:s, halve the number of steps and 
# create the next granularity of eta and W.
for i in range(1): # len(h)
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
