import numpy as np

mu, sigma = 30, 5 # mean and standard deviation
s = np.random.normal(mu, sigma, 100000)
for i in s:
  print(i)