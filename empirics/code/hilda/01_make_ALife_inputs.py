import pandas as pd
import numpy as np
from functools import reduce

# Imports
from directories import DATAPATH, OUTPATH
from fxns_globals import clip_outlier

# Minimum and maximum ages for all years
min_age = 23
max_age = 64

# Collect data from HILDA waves
waves = {'b':2002, 'c':2003, 'd':2004, 'e':2005,
         'f':2006, 'g':2007, 'h':2008, 'i':2009,
         'j':2010, 'k':2011, 'l':2012, 'm':2013,
         'n':2014, 'o':2015, 'p':2016, 'q':2017,
         'r':2018, 's':2019}
keeps = ['xwaveid', 'year', 'hours', 'loghours', 'occupation1', 
         'occupation2', 'cs_weight', 'lg_weight']
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
    # Calculate hours worked
    df['hours'] = pd.to_numeric(df[f'{w}jbhruc'], errors = 'coerce')
    df['loghours'] = np.log(df['hours'].mask(df['hours'] <= 0))
    # Calculate occupation
    df['occupation1'] = df[f'{w}jbmo61']
    df['occupation2'] = df[f'{w}jbmo62']
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
    
# Concatenate and create lags
df = pd.concat(dfs, ignore_index = True).sort_values(['xwaveid', 'year'])
for x in [y for y in keeps if y != 'xwaveid' and y != 'year' and 'weight' not in y]:
    df['l_' + x] = df.groupby('xwaveid')[x].shift(1)
for v in ['hours', 'loghours']:
    df['d_' + v] = df[v] - df['l_' + v]
df['d_occupation1'] = (df['occupation1'] != df['l_occupation1']).astype(int)
df['d_occupation2'] = (df['occupation2'] != df['l_occupation2']).astype(int)

# Drop missings, which will for sure drop first year
df = df.dropna()

# Function to calculate standard deviation with weights
def weighted_std(x, w):
    mu = np.average(x, weights = w)
    std = np.sqrt(np.average((x - mu)**2, weights = w))
    return std

# Cleaning
hvar = 'loghours'
ocvar = 'occupation2'
dfs = clip_outlier(df, hvar, 0.01, 0.99) # winsorize extremes
dfs = dfs.query(f'd_{ocvar} == 0') # restrict to no job changes

# Calculate summary statistics
count = dfs.groupby(ocvar)[hvar].count().reset_index()
level_uw = dfs.groupby(ocvar)[hvar].std().reset_index()
level = dfs.groupby(ocvar).apply(lambda x: weighted_std(x[hvar], x['cs_weight'])).reset_index()
chg_unw = dfs.groupby(ocvar)['d_' + hvar].std().reset_index()
chg = dfs.groupby(ocvar).apply(lambda x: weighted_std(x['d_' + hvar], x['lg_weight'])).reset_index()

# Combine results
res = reduce(lambda x,y: pd.merge(x, y, on = ocvar), [count, level_uw, level, chg_unw, chg])
res = res.set_index(ocvar).sort_index()
res.columns = ['count', 'level_uw', 'level', 'chg_uw', 'chg']

# Output, formatting occupation variable
res = res.reset_index()
res[f'{ocvar}_code'] = res[ocvar].apply(lambda x: str(x).split(']')[0][1:])
res[f'{ocvar}_label'] = res[ocvar].apply(lambda x: str(x).split(']')[-1])
res = res.drop(columns = ocvar)
res.to_csv(OUTPATH / f'sd{hvar}_{ocvar}.csv', index = False)
