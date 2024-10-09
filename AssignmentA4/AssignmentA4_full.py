import random
import numpy as np
import matplotlib.pyplot as plt
import time 

# QUESTION 1

start_time = time.time()
plt.figure()

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


print("DONE Q1 in %.2f seconds" % (time.time() - start_time))




# QUESTION 2
start_time = time.time()
plt.figure()

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


print("DONE Q2 in %.2f seconds" % (time.time() - start_time))




# QUESTION 3
start_time = time.time()
plt.figure()

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


print("DONE Q3 in %.2f seconds" % (time.time() - start_time))




# QUESTION 4
start_time = time.time()
plt.figure()

# Create h:
h = np.array([2**-i for i in range(10, 0, -1)])
number_steps = int(1/min(h))


# Set constants
M = 1000
mu = 2
sigma = 1
exp_X_1 = np.exp(mu)
X_last = np.zeros([M, len(h)])
random.seed("felix")

for m in range(M):
    # Generate all eta:s for the smallest level
    eta_finest = np.zeros(number_steps)
    for e in range(number_steps):
    # Normal distribution, give it sigma not sigma^2 
        eta_finest[e] = random.gauss(0, np.sqrt(min(h)))
    
    # For the exact X, we use the small timesteps
    timesteps_finest = np.array([n/number_steps for n in range(number_steps)])
    #Define constants and create necessary empty arrays of the correct size
    W_finest = np.zeros(number_steps+1)
    X_exact = np.zeros(number_steps+1)

    # Create the W for the finest level
    for j in range(1, number_steps+1):
        W_finest[j] = W_finest[j-1] + eta_finest[j-1]
    # Calculate exact X for the generated Brownian motion
    X_exact = np.exp((mu-(sigma**2)/2) + sigma*W_finest)


    #Init loop
    eta_previous = eta_finest

    for i in range(len(h)):
        # Create empty arrays of the correct size for this resolution
        level_size = int(len(eta_previous)/2)
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

        # Save the last X to be used to calculate error
        X_last[m, i] = X_level[-1]
        
        # Prepare for next loop
        eta_previous = eta_next

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
plt.savefig("figures/q4.png")


print("DONE Q4 in %.2f seconds" % (time.time() - start_time))




# QUESTION 5
start_time = time.time()
plt.figure()

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
    #Define constants and create necessary empty arrays
    W_finest = np.zeros(number_steps+1)
    X_exact = np.zeros(number_steps+1)

    # Create the W for the finest level
    for j in range(1, number_steps+1):
        W_finest[j] = W_finest[j-1] + eta_finest[j-1]
    X_exact = np.exp((mu-(sigma**2)/2) + sigma*W_finest)


    #Init loop
    eta_previous = eta_finest

    for i in range(len(h)):
        # Create empty arrays of the correct size for this resolution
        level_size = int(len(eta_previous)/2)
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
        
        # Save the difference between the "true" value and the approximation
        diff_X[m, i] = (X_exact[-1])**2 - (X_level[-1])**2

        # Prepare for next loop
        eta_previous = eta_next

# Calculate the weak error
weak_errors = np.sqrt(sum((diff_X)**2))/M

# Plot and save
plt.loglog(h, weak_errors, 'o', label = "Weak Error")
plt.loglog(h, np.sqrt(h), label = "sqrt(h)")
plt.xlabel("h")
plt.ylabel("Error")
plt.legend()
plt.title("Question 5")
plt.savefig("figures/q5.png")


print("DONE Q5 in %.2f seconds" % (time.time() - start_time))
