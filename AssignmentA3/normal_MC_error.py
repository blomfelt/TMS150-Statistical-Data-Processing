# -*- coding: utf-8 -*-
"""
Created on Thu Sep 19 10:14:18 2024

@author: ioann
"""
# Import libraries
import random
import numpy as np
import matplotlib.pyplot as plt

# Set variables
mu = 0
sigma = 1

N = 20

# Allocate vector
sample_size = np.zeros(N)

for n in range(N):
    sample_size[n] = 2**(n+1)
    
print(sample_size)

# Allocate new vectors
average = np.zeros(N)
error_av = np.zeros(N)
average_1 = 0

for m in range(int(sample_size[N-1])):
    average_1 += random.gauss(mu,sigma)
    for n in range(N):
        if m == (int(sample_size[n])-1):
            average[n] = average_1/sample_size[n]
            error_av[n] = np.abs(average[n]-mu)
            
print(error_av)

# %%
# Plotting of the error
plt.loglog(sample_size, error_av)
plt.savefig('figures/error.png')

plt.loglog(sample_size,1/np.sqrt(sample_size))

# %%
# Save figure as pdf file with better quality
plt.savefig('normal_error.pdf')
# %%

