import random
import numpy as np
import matplotlib.pyplot as plt
import time 

# Question 1
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
W[0] = np.append([0], np.cumsum(eta_finest))
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


print("DONE Q1 in %.2f seconds" % (time.time() - start_time))

#plt.show()



# QUESTION 2
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
W[0] = np.append([0], np.cumsum(eta_finest))
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


# Create the X_exact using the finest level W
X_exact = np.exp((mu-(sigma**2)/2)*timesteps[0] + sigma*W[0])
# Plot it
plt.plot(timesteps[0], X_exact, label = "True X")

# Create the approximation X for each level
X_all = {} 
for i in range(len(W)):
    # Extract relevant values for this level
    h_level = h[i]
    level_size = int(1/h[i]+1)
    W_level = W[i]

    # Create empty X of correct sixe for this level
    X_level = np.zeros(level_size)
    
    # Set initial X[0] = 1
    X_level[0] = 1
    # Loop over all values and calculate approximation
    for n in range(1, level_size):
        X_level[n] = ( (1 + h_level * mu) * X_level[n-1] + 
            sigma * X_level[n-1] * (W_level[n] - W_level[n-1]) )
    # Save current level
    X_all[i] = X_level

# Plot all the approximations:
for i in range(len(W)):
    plt.plot(timesteps[i], X_all[i], label = "i = %s" % (10 - i))

# Format plot and save it
plt.title("Question 2")
plt.xlabel("t")
plt.ylabel("X(t)")
plt.legend()
plt.savefig("figures/q2.png")

print("DONE Q2 in %.2f seconds" % (time.time() - start_time))

#plt.show()



# QUESTION 3
plt.figure()
start_time = time.time()

# Create h:
h = np.array([2**-i for i in range(10, 0, -1)])
number_steps = int(1/min(h))

#Define constants and create necessary empty arrays
mu = 2
sigma = 1
M = 1000
diff_X = np.zeros([M, len(h)])

random.seed("felix")
for m in range(M):
    # Generate all eta:s for the smallest level
    eta_finest = np.zeros(number_steps)
    for e in range(number_steps):
        # Normal distribution, give it sigma not sigma^2
        eta_finest[e] = random.gauss(0, np.sqrt(min(h)))
    
    # Create empty sets
    W = {}
    timesteps = {}
    # Manually create the first/finest level
    W[0] = np.append([0], np.cumsum(eta_finest))
    # Create timesteps from 0 to 1 i.e. grid size + 1
    timesteps[0] = np.array([n*h[0] for n in range(int(1/h[0]+1))])
    
    # Generate all other levels of the grid, starting from the second finest
    # Init loop
    eta_finer = eta_finest
    # Loop over all other levels
    for e in range(1, len(h)):
        eta_coarser = eta_finer[::2] + eta_finer[1::2]
        W[e] = np.append([0], np.cumsum(eta_coarser))
        # Create the correct timesteps while we loop over, but note that these 
        # are 1 loop/step after, i.e. when W have 513 elements, timesteps have
        # 1025 
        timesteps[e] = np.array([n*h[e] for n in range(int(1/h[e]+1))])
        eta_finer = eta_coarser
    
    
    # Create the X_exact using the finest level W
    X_exact = np.exp((mu-(sigma**2)/2)*timesteps[0] + sigma*W[0])
    # Plot it
    #plt.plot(timesteps[0], X_exact, label = "True X")
    
    # Create the approximation X for each level
    X_all = {} 
    for i in range(len(W)):
        # Extract relevant values for this level
        h_level = h[i]
        level_size = int(1/h[i]+1)
        W_level = W[i]
    
        # Create empty X of correct sixe for this level
        X_level = np.zeros(level_size)
        
        # Set initial X[0] = 1
        X_level[0] = 1
        # Loop over all values and calculate approximation
        for n in range(1, level_size):
            X_level[n] = ( (1 + h_level * mu) * X_level[n-1] + 
                sigma * X_level[n-1] * (W_level[n] - W_level[n-1]) )
        # Calculate difference of current level and save
        diff_X[m, i] = X_exact[-1] - X_level[-1]

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

print("DONE Q3 in %.2f seconds" % (time.time() - start_time))

#plt.show()



# QUESTION 4
plt.figure()
start_time = time.time()

# Create h:
h = np.array([2**-i for i in range(10, 0, -1)])
number_steps = int(1/min(h))

#Define constants and create necessary empty arrays
mu = 2
sigma = 1
M = 1000
X_last = np.zeros([M, len(h)])
exp_X_1 = np.exp(mu)

random.seed("felix")
for m in range(M):
    # Generate all eta:s for the smallest level
    eta_finest = np.zeros(number_steps)
    for e in range(number_steps):
        # Normal distribution, give it sigma not sigma^2
        eta_finest[e] = random.gauss(0, np.sqrt(min(h)))
    
    # Create empty sets
    W = {}
    timesteps = {}
    # Manually create the first/finest level
    W[0] = np.append([0], np.cumsum(eta_finest))
    # Create timesteps from 0 to 1 i.e. grid size + 1
    timesteps[0] = np.array([n*h[0] for n in range(int(1/h[0]+1))])
    
    # Generate all other levels of the grid, starting from the second finest
    # Init loop
    eta_finer = eta_finest
    # Loop over all other levels
    for e in range(1, len(h)):
        eta_coarser = eta_finer[::2] + eta_finer[1::2]
        W[e] = np.append([0], np.cumsum(eta_coarser))
        # Create the correct timesteps while we loop over, but note that these 
        # are 1 loop/step after, i.e. when W have 513 elements, timesteps have 
        # 1025 
        timesteps[e] = np.array([n*h[e] for n in range(int(1/h[e]+1))])
        eta_finer = eta_coarser
  
    # Create the X_exact using the finest level W
    X_exact = np.exp((mu-(sigma**2)/2)*timesteps[0] + sigma*W[0])
    # Plot it
    #plt.plot(timesteps[0], X_exact, label = "True X")
    
    # Create the approximation X for each level
    X_all = {} 
    for i in range(len(W)):
        # Extract relevant values for this level
        h_level = h[i]
        level_size = int(1/h[i]+1)
        W_level = W[i]
    
        # Create empty X of correct sixe for this level
        X_level = np.zeros(level_size)
        
        # Set initial X[0] = 1
        X_level[0] = 1

        # Loop over all values and calculate approximation
        for n in range(1, level_size):
            X_level[n] = ( (1 + h_level * mu) * X_level[n-1] + 
                sigma * X_level[n-1] * (W_level[n] - W_level[n-1]) )
        
        # Obtain last value and save
        X_last[m, i] = X_level[-1]

# Sum each column (each resolution) and divide by M
x_h = sum(X_last)/M

# Obtain the weak errors, one for each resolution
weak_errors = abs(exp_X_1 - x_h)


# Plot and save
plt.loglog(h, weak_errors, 'o', label = "Weak Error")
plt.loglog(h, np.sqrt(h), label = "sqrt(h)")
plt.xlabel("h")
plt.ylabel("Error")
plt.legend()
plt.title("Question 4")
plt.savefig("figures/q4_%i.png" % M)

print("DONE Q4 in %.2f seconds" % (time.time() - start_time))

#plt.show()



# QUESTION 5
plt.figure()
start_time = time.time()

# Create h:
h = np.array([2**-i for i in range(10, 0, -1)])
number_steps = int(1/min(h))

#Define constants and create necessary empty arrays
mu = 2
sigma = 1
M = 1000
X_last = np.zeros([M, len(h)])

random.seed("felix")
for m in range(M):
    # Generate all eta:s for the smallest level
    eta_finest = np.zeros(number_steps)
    for e in range(number_steps):
        # Normal distribution, give it sigma not sigma^2
        eta_finest[e] = random.gauss(0, np.sqrt(min(h)))
    
    # Create empty sets
    W = {}
    timesteps = {}
    # Manually create the first/finest level
    W[0] = np.append([0], np.cumsum(eta_finest))
    # Create timesteps from 0 to 1 i.e. grid size + 1
    timesteps[0] = np.array([n*h[0] for n in range(int(1/h[0]+1))])
    
    # Generate all other levels of the grid, starting from the second finest
    # Init loop
    eta_finer = eta_finest
    # Loop over all other levels
    for e in range(1, len(h)):
        eta_coarser = eta_finer[::2] + eta_finer[1::2]
        W[e] = np.append([0], np.cumsum(eta_coarser))
        # Create the correct timesteps while we loop over, but note that these 
        # are 1 loop/step after, i.e. when W have 513 elements, timesteps have 
        # 1025 
        timesteps[e] = np.array([n*h[e] for n in range(int(1/h[e]+1))])
        eta_finer = eta_coarser
    
    
    # Create the X_exact using the finest level W
    X_exact = np.exp((mu-(sigma**2)/2)*timesteps[0] + sigma*W[0])
    # Plot it
    #plt.plot(timesteps[0], X_exact, label = "True X")
    
    # Create the approximation X for each level
    X_all = {} 
    for i in range(len(W)):
        # Extract relevant values for this level
        h_level = h[i]
        level_size = int(1/h[i]+1)
        W_level = W[i]
    
        # Create empty X of correct sixe for this level
        X_level = np.zeros(level_size)
        
        # Set initial X[0] = 1
        X_level[0] = 1
        # Loop over all values and calculate approximation
        for n in range(1, level_size):
            X_level[n] = ( (1 + h_level * mu) * X_level[n-1] + 
                sigma * X_level[n-1] * (W_level[n] - W_level[n-1]) )
        
        # Obtain last value and save
        # Also apply the the test function to it
        X_last[m, i] = np.sqrt(abs(X_level[-1]))

# Sum each column (each resolution) and divide by M
x_h = sum(X_last)/M

# Obtain the weak errors, one for each resolution
weak_errors = abs(np.exp(0.5*mu - sigma/8) - x_h)


# Plot and save
plt.loglog(h, weak_errors, 'o', label = "Weak Error")
plt.loglog(h, np.sqrt(h), label = "sqrt(h)")
plt.xlabel("h")
plt.ylabel("Error")
plt.legend()
plt.title("Question 5")
plt.savefig("figures/q5.png")

print("DONE Q5 in %.2f seconds" % (time.time() - start_time))

plt.show()
