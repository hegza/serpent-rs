import numpy as np
import csv

n = 9_000_000

S = np.random.rand(n) * 50 + 25
K = np.random.rand(n) * 100 + 50
T = np.random.rand(n) * 1 + 0.5
r = np.random.rand(n) * 0.05 + 0.025
sigma = np.random.rand(n) * 0.25 + 0.175

with open('dataset.csv', mode='w') as f:
    w = csv.writer(f, delimiter=',')

    for i in range(n):
        w.writerow([S[i], K[i], T[i], r[i], sigma[i]])

