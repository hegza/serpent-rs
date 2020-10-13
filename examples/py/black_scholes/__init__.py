# External imports
import numpy as np
import csv
import pandas
import time

from black_scholes import euro_vanilla_put, euro_vanilla_call


def main():
    dataset = pd.read_csv('dataset.csv', sep=',', header=None).to_numpy()
    
    n = dataset.shape[0]
    S = dataset[:,0]
    K = dataset[:,1]
    T = dataset[:,2]
    r = dataset[:,3]
    sigma = dataset[:,4]
    
    start_time = time.time()
    euro_vanilla_put(S, K, T, r, sigma)
    euro_vanilla_call(S, K, T, r, sigma)
    duration = time.time() - start_time
    
    print("Calculating put and call for", n, "options took", duration, "seconds")


if __name__ == "__main__":
    main()


