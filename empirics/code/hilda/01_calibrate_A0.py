import pandas as pd
import numpy as np

# Imports
from directories import DATAPATH, FORPATH
from fxns_globals import indexation_t, series_txt, clip_outlier

# Minimum and maximum ages for all years
min_age = 18
max_age = 22

# Function to adjust for inflation to 2005 dollars using HELP threshold indexation
def adjust_infl(y, year):
    level_05 = np.product([indexation_t[x] for x in range(1991,2006)])
    level_yr = np.product([indexation_t[x] for x in range(1991,year + 1)])
    return y * level_05 / level_yr

# Collect data from HILDA waves
waves = {'b':2002, 'f':2006, 'j':2010, 'n':2014, 'r':2018} # wave index p.8 of 1.Readme200.pdf
keeps = ['year', 'age', 'type', 'netassets', 'helpdebt', 'hasdebt', 'cs_weight'] # variables to keep
infl_vars = ['netassets', 'helpdebt'] # variables to adjust for inflation
dfs = []
for w in waves:
    # Read in merged data at household level
    df = pd.read_stata(DATAPATH / f'Combined_{w}200c.dta')
    # Filters
    df[f'{w}hgage1'] = pd.to_numeric(df[f'{w}hgage1'], errors = 'coerce')
    df = df.query(f'{w}hgage1 >= @min_age and {w}hgage1 <= @max_age') # age range
    df = df[df[f'{w}hhtype'] == '[24] Lone person'] # keep lone individuals only
    df['age'] = df[f'{w}hgage1']
    df['type'] = df[f'{w}hhtype']
    # Calculate net assets using liquid wealth variables (using all imputed variables, see p.85 User Manual)
    df['netassets'] = (
        ## Assets
        df[f'{w}hwtbani'].astype(float) + # bank accounts
        df[f'{w}hwcaini'].astype(float) + # cash, money market, and debt investments
        df[f'{w}hweqini'].astype(float) - # equity investments
        ## Debts
        df[f'{w}hwccdti'].astype(float) - # credit card debt
        df[f'{w}hwothdi'].astype(float)   # other personal debt (car, investment, or personal loans)
        )
    # Calculate whether individuals have HELP debt
    df['helpdebt'] = df[f'{w}hwhecdi'].astype(float)
    df['hasdebt'] = (df['helpdebt'] > 0).astype(int)
    # Adjust for inflation
    df[infl_vars] = df[infl_vars].apply(adjust_infl, args = (waves[w],))
    # Adjust cross-sectional survey weights so they sum to one in each year
    df['cs_weight'] = df[f'{w}hhwth'].astype(float) / df[f'{w}hhwth'].astype(float).sum()
    # Append
    df['year'] = waves[w]
    dfs.append(df[keeps])
    
# Concatenate and winsorize
df = pd.concat(dfs, ignore_index = True)
df = clip_outlier(df, ['netassets'], p_low = 0.01, p_high = 0.99)

# Function to calculate weighted mean and std (for normal, same as norm.fit)
def fit_weighted_normal(x, w):
    mu = np.average(x, weights = w)
    std = np.sqrt(np.average((x - mu)**2, weights = w))
    return mu, std

# Function to estimate initial wealth distribution as log-normal with probability of zero
def estimate_distribution(data_, var, wvar, lowval = 0):
    data = data_.copy()
    data[var] = data[var].clip(lower = lowval)
    p0 = (data[var] == lowval).astype(int).mean()
    data = data[data[var] > lowval].copy()
    data[var] = np.log(data[var])
    data[var].hist()
    mu, sd = fit_weighted_normal(data[var], data[wvar])
    max = data[var].max()
    return [p0, mu, sd, max]

# Fit distributions
series_txt(pd.Series(estimate_distribution(df[df.hasdebt == 1], 'netassets', 'cs_weight')), 
           FORPATH / 'initial_assets_d.txt')
series_txt(pd.Series(estimate_distribution(df[df.hasdebt == 0], 'netassets', 'cs_weight')), 
           FORPATH / 'initial_assets_nd.txt')
