import pandas as pd
import numpy as np

# Imports
from directories import DATAPATH, FORPATH
from fxns_globals import indexation_t, series_txt, clip_outlier
from fxns_bootstrap import bootstrap_statistic

# Set seeds
np.random.seed(33)

# Minimum and maximum ages for all years
min_age = 22
max_age = 64

# Minimum total work hours per week
min_hours = 19 # half-time

# Function to adjust for inflation to 2005 dollars using HELP threshold indexation
def adjust_infl(y, year):
    level_05 = np.product([indexation_t[x] for x in range(1991,2006)])
    level_yr = np.product([indexation_t[x] for x in range(1991,year + 1)])
    return y * level_05 / level_yr

# Collect data from HILDA waves
hvars = ['hours', 'loghours'] # hours variables
waves = {'b':2002, 'c':2003, 'd':2004, 'e':2005, # all waves
         'f':2006, 'g':2007, 'h':2008, 'i':2009,
         'j':2010, 'k':2011, 'l':2012, 'm':2013,
         'n':2014, 'o':2015, 'p':2016, 'q':2017,
         'r':2018, 's':2019} # wave index p.8 of 1.Readme200.pdf
waves_wealth = ['b', 'f', 'j', 'n', 'r'] # waves with wealth module
keeps = ['xwaveid', 'year', 'age', 'cs_weight', 'lg_weight', 'netassets', 'networth', 
         'wage', 'logwage', 'income'] + hvars # variables to keep
dfs = []
lgwgt = pd.read_stata(DATAPATH / 'longitudinal_weights_t200c.dta')
for i,w in enumerate(waves):
    # Read in merged data at household level
    df = pd.read_stata(DATAPATH / f'Combined_{w}200c.dta')
    # Filters
    df[f'{w}hgage1'] = pd.to_numeric(df[f'{w}hgage1'], errors = 'coerce')
    df = df.query(f'{w}hgage1 >= @min_age and {w}hgage1 <= @max_age') # age range
    df = df[df[f'{w}esbrd'] == '[1] Employed'] # employed
    df = df.query(f'{w}wscei > 0') # earning a weekly wage
    df = df[df[f'{w}bifhave'] == '[2] No'] # not a business owner
    # Get age
    df['age'] = df[f'{w}hgage1']
    # Calculate net assets and net worth using liquid wealth variables (using all imputed variables, see p.85 User Manual)
    if w in waves_wealth:
        df['netassets'] = (
            ## Assets
            df[f'{w}hwtbani'].astype(float) + # bank accounts
            df[f'{w}hwcaini'].astype(float) + # cash, money market, and debt investments
            df[f'{w}hweqini'].astype(float) - # equity investments
            ## Debts
            df[f'{w}hwccdti'].astype(float) - # credit card debt
            df[f'{w}hwothdi'].astype(float)   # other personal debt (car, investment, or personal loans)
            ).apply(adjust_infl, args = (waves[w],))
        df['networth'] = (df[f'{w}hwnwip'].astype(float) - 
                          df[f'{w}hwnwin'].astype(float)).apply(adjust_infl, args = (waves[w],))
    else:
        df['netassets'] = np.nan
        df['networth'] = np.nan
    # Calculate hours worked
    df['hours'] = pd.to_numeric(df[f'{w}jbhruc'], errors = 'coerce')
    df['loghours'] = np.log(df['hours'].mask(df['hours'] <= 0))
    # Get hourly wage rate as reccomended by HILDA manual p.54
    df['wage'] = pd.to_numeric(df[f'{w}wscei'], errors = 'coerce') / df['hours']
    df['logwage'] = np.log(df['wage'].mask(df['wage'] <= 0))
    # Store annual income
    df['income'] = df[f'{w}hiwsfei'].astype(float).apply(adjust_infl, args = (waves[w],))
    # Adjust cross-sectional survey weights so they sum to one in each year
    df['cs_weight'] = df[f'{w}hhwth'].astype(float) / df[f'{w}hhwth'].astype(float).sum()
    # Merge in longitudinal weight from previous year, excluding first year
    if i > 0:
        lg_weight = f'wlr{list(waves.keys())[i - 1]}{w}'
        lgwgt_t = lgwgt[['xwaveid', lg_weight]].copy().rename(columns = {lg_weight:'lg_weight'})
        df = df.merge(lgwgt_t, how = 'left', on = 'xwaveid')
    else:
        df['lg_weight'] = np.nan
    # Append
    df['xwaveid'] = df['xwaveid'].astype(int)
    df['year'] = waves[w]
    dfs.append(df[keeps])
    
# Concatenate
df = pd.concat(dfs, ignore_index = True).sort_values(['xwaveid', 'year'])
df = clip_outlier(df, hvars + ['netassets', 'networth', 'wage', 'logwage', 'income'], 0.01, 0.99) # winsorize extremes

# Calculate change in hours and log hours
for v in ['hours','loghours']:
    df['d_' + v] = df[v] - df.groupby('xwaveid')[v].shift(1)

# Filter out people with insufficient hours
df = df.query('hours >= @min_hours')

# Average hours and standard deviation used for model
df0 = df.dropna(subset = ['hours', 'lg_weight']).copy()
mean_h = np.average(df0['hours'], weights = df0['lg_weight'])
sd_h = np.sqrt(np.average((df0['hours'] - mean_h)**2, weights = df0['lg_weight']))
se_h = sd_h/np.sqrt(len(df0))
print(f'Mean hours: {mean_h}, SE hours: {se_h}')
se_mean = se_h/mean_h
series_txt(pd.Series(se_mean), FORPATH / 'SEs/hours_mean.txt')

# Kurtosis of difference in log hours
def weighted_kurtosis(x, w):
    mu = np.average(x, weights = w)
    std = np.sqrt(np.average((x - mu)**2, weights = w))
    kurtosis = np.average((x - mu)**4, weights = w)
    return kurtosis / (std**4) - 3
n_long = 1
df0_l = df.copy()
df0_l['year'] = df0_l['year'] + n_long
df0_l = df0_l.rename(columns = {'hours':'lag_hours'})
df0 = df.merge(df0_l[['xwaveid', 'year', 'lag_hours']], on = ['xwaveid', 'year'])
df0['d_loghours'] = np.log(df0['hours'] / df0['lag_hours'])
series_txt(pd.Series(weighted_kurtosis(df0['d_loghours'], df0['lg_weight'])), FORPATH / 'hours_kurtosis.txt')
## Standard error
se_kurt = bootstrap_statistic(df0, lambda x: weighted_kurtosis(x['d_loghours'], x['lg_weight']), 1000)
series_txt(pd.Series(se_kurt), FORPATH / 'SEs/hours_kurtosis.txt')

# Calculate fraction of people with less than one hour adjustment in hours worked
df0['no_adj'] = (np.abs(df0['hours'] - df0['lag_hours']) <= 1).astype(int)
series_txt(pd.Series(np.average(df0['no_adj'], weights = df0['lg_weight'])), FORPATH / 'fraction_noadjust.txt')
## Standard error
se_adj = bootstrap_statistic(df0, lambda x: np.average(x['no_adj'], weights = x['lg_weight']), 1000)
series_txt(pd.Series(se_adj), FORPATH / 'SEs/fraction_noadjust.txt')
