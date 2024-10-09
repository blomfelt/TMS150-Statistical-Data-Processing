import random
import numpy as np
import matplotlib.pyplot as plt
import time 

start_time = time.time()

# Create h:
h = np.array([2**-i for i in range(1, 11)])
number_steps = int(1/min(h))

# Generate all eta:s for the smallest level
eta_finest = np.zeros(number_steps)
random.seed("felix")
for e in range(number_steps):
    # Normal distribution, give it sigma not sigma^2
    eta_finest[e] = random.gauss(0, np.sqrt(min(h)))

#Init loop
eta_previous = eta_finest
level_size = number_steps
# Use a loop which uses the previous eta:s, halve the number of steps and 
# create the next granularity of eta and W.
for i in range(len(h)):
    # Sum every two eta:s to form the next level
    level_size = int(len(eta_previous)/2)
    eta_next = np.zeros(level_size)
    W_level = np.zeros(level_size+1)
    timesteps_level = np.array([n/level_size for n in range(level_size+1)])
    for e in range(level_size):
        eta_next[e] = eta_previous[e*2] + eta_previous[e*2+1]
    
    # Create the next W using the previous and the corresponding eta
    for i in range(1, level_size+1): #Level size +1 since non-inclusive range
        W_level[i] = W_level[i-1] + eta_next[i-1]
    plt.plot(timesteps_level, W_level)

    # Prepare for next loop
    eta_previous = eta_next

#Plot it and save it
plt.title("Question 1")
plt.xlabel("t")
plt.ylabel("W(t)")
plt.savefig("figures/q1.png")


print("DONE in %.2f seconds" % (time.time() - start_time))
