import pandas as pd
import numpy as np
import statsmodels.api as sm

# Imports
from directories import DATAPATH, FORPATH
from fxns_globals import indexation_t, series_txt, clip_outlier

# Minimum and maximum ages for all years
min_age = 22
max_age = 90

# Function to adjust for inflation to 2005 dollars using HELP threshold indexation
def adjust_infl(y, year):
    level_05 = np.product([indexation_t[x] for x in range(1991,2006)])
    level_yr = np.product([indexation_t[x] for x in range(1991,year + 1)])
    return y * level_05 / level_yr

# Collect data from HILDA waves
waves = {'b':2002, 'f':2006, 'j':2010, 'n':2014, 'r':2018} # wave index p.8 of 1.Readme200.pdf
keeps = ['year', 'age', 'cc_limit', 'netassets', 'helpdebt', 'hasdebt', 'cs_weight'] # variables to keep
infl_vars = ['cc_limit', 'netassets', 'helpdebt'] # variables to adjust for inflation
dfs = []
for w in waves:
    # Read in merged data at household level
    df = pd.read_stata(DATAPATH / f'Combined_{w}200c.dta')
    # Filters
    df[f'{w}hgage1'] = pd.to_numeric(df[f'{w}hgage1'], errors = 'coerce')
    df = df.query(f'{w}hgage1 >= @min_age and {w}hgage1 <= @max_age') # age range
    df['age'] = df[f'{w}hgage1']
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
    # Calculate credit card limit and keep people with non-missing values
    df['cc_limit'] = -1 * pd.to_numeric(df[f'{w}crymbl'], errors = 'coerce')
    df = df.dropna(subset = ['cc_limit'])
    # Adjust for inflation
    df[infl_vars] = df[infl_vars].apply(adjust_infl, args = (waves[w],))
    # Adjust cross-sectional survey weights so they sum to one in each year
    df['cs_weight'] = df[f'{w}hhwth'].astype(float) / df[f'{w}hhwth'].astype(float).sum()
    # Append
    df['year'] = waves[w]
    dfs.append(df[keeps])
    
# Concatenate and winsorize
df = pd.concat(dfs, ignore_index = True)
df = clip_outlier(df, ['netassets', 'cc_limit'], p_low = 0.01, p_high = 0.99)

# Average by age and fitted regression
df['age2'] = df['age']**2
df['age3'] = df['age']**3
df['age4'] = df['age']**4
X = sm.add_constant(df[['age', 'age2', 'age3', 'age4']])
Y = df['cc_limit']
mod = sm.WLS(Y, X, weights = df['cs_weight']).fit()
series_txt(mod.params, FORPATH / 'cc_limit_agemodel.txt')
