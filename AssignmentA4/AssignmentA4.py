import random
import numpy as np
import matplotlib.pyplot as plt
#import time 

# Create h:
h = np.array([2**-i for i in range(1, 11)])
number_steps = int(1/min(h))
#print(h)

# Generate all eta:s for the smallest level
eta_finest = np.zeros(number_steps)
random.seed("felix")
for e in range(number_steps):
    eta_finest[e] = random.gauss(0, min(h))
# Generate all timesteps for the finest level:
#timesteps = np.array([n/number_steps for n in range(number_steps+1)])

# Create finest level
#W = np.zeros(number_steps+1)
#for n in range(1, number_steps):
#    W[n] = W[n-1] + eta_finest[n-1]
#plt.plot(timesteps, W)


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

    eta_previous = eta_next

#TODO: Add title
plt.title("")
plt.savefig("figures/q1.png")


print("DONE")
#print("Finest")
#print(eta_finest)
#print("Next")
#print(eta_next)


print(W_level)



exit()
for i in range(len(h)):
    N = int(1/h[i])
    # Lenght N+1 since there is N time steps in it, and 0 is start 
    t = np.zeros(N+1)
    t = np.array([n*h[i] for n in range(N+1)])
    #print("\nt = " + str(t))
    #for time_next in t 
