# External imports
import numpy as np
import scipy.stats as si

from black_scholes_ndp import euro_vanilla_put


def main():
    put = euro_vanilla_put(50, 100, 1, 0.05, 0.25)
    print("put: " + str(put))

    n = 100
    S = np.random.rand(n) * 50 + 25
    K = np.random.rand(n) * 100 + 50
    T = np.random.rand(n) * 1 + 0.5
    r = np.random.rand(n) * 0.05 + 0.025
    sigma = np.random.rand(n) * 0.25 + 0.175

    result = euro_vanilla_put(S, K, T, r, sigma)
    print("result: " + str(result))


if __name__ == "__main__":
    main()
