import numpy as np
from sklearn.utils import resample
import random

random.seed(33)

# Function to make one bootstrap sample of dataframe
def bootstrap_sample(df):
    return resample(df, replace = True)

# Function to calculate bootstrap statistic
def bootstrap_statistic(df, f:callable, Nb:int):
    truth = f(df)
    results = np.zeros(Nb)
    for b in range(Nb):
        df_bs = bootstrap_sample(df)
        results[b] = f(df_bs)
    sd = np.sqrt(np.mean((results - truth)**2))
    return sd

# Function to bootstrap a vector of statistics
def bootstrap_vector(df, f:callable, Nb:int):
    truth = f(df)
    N = len(truth)
    results = np.zeros((Nb, N))
    for b in range(Nb):
        df_bs = bootstrap_sample(df)
        results[b,:] = f(df_bs)
    sd = np.sqrt(np.mean((results - truth)**2, axis = 0))
    return sd
    
    