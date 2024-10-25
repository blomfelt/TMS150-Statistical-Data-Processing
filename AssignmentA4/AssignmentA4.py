import random
import numpy as np
import matplotlib.pyplot as plt
import time 

plt.figure()
start_time = time.time()

# Create h:
h = np.array([2**-i for i in range(10, 0, -1)])
number_steps = int(1/min(h))

#Define constants and create necessary empty arrays
mu = 2
sigma = 1

# Generate all eta:s for the smallest level
eta_finest = np.zeros(number_steps)
random.seed("felix")
for e in range(number_steps):
    # Normal distribution, give it sigma not sigma^2
    eta_finest[e] = random.gauss(0, np.sqrt(min(h)))

# Create empty sets
W = {}
timesteps = {}
# Manually create the first/finest level
W[0] = np.concatenate(([0], np.cumsum(eta_finest)))
# Create timesteps from 0 to 1 i.e. grid size + 1
timesteps[0] = np.array([n*h[0] for n in range(int(1/h[0]+1))])

# Generate all other levels of the grid, starting from the second finest
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

# Plot all the approximations:
for i in range(len(W)):
    plt.plot(timesteps[i], W[i], label = "i = %s" % (10 - i))


#Plot it and save it
plt.title("Question 1")
plt.xlabel("t")
plt.ylabel("W(t)")
plt.savefig("figures/q1.png")


print("DONE in %.2f seconds" % (time.time() - start_time))
plt.show()
