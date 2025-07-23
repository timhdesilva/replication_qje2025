import os
import pandas as pd
import numpy as np
from scipy.stats import norm

# Imports
from directories import datadir, momdir, file_debt
from fxns_globals import series_txt

###############################################################################
# FILE PARAMETERS
###############################################################################

# Important variables
idvar = 'alife_id_001' # unique individual identifier
tvar = 'income_year' # variable for income year

###############################################################################
# MERGE DEBT BALANCES AND HELP INCOME INFORMATION
###############################################################################

# Variables to keep from help_.dta files
keepvars = [idvar, tvar, 'help_income', 'hc_repayment', 'help_reqpay', 'help_debt_bal_l1',
            'new_help', 'indexation_d', 'dindex_05', 'c_age_30_june']

# Read in debt balances
df = pd.read_csv(file_debt)
df[idvar] = df[idvar].astype(float) # convert to format of aLife10_tax files
df['help_debt_bal'] = np.maximum(df['help_debt_bal'], 0)

# Keep people who ever have debt
df['max_debt'] = df.groupby(idvar)['help_debt_bal'].transform(max)
df = df[df.max_debt > 0]

# Merge in income in each year
os.chdir(datadir)
files = [x for x in os.listdir(os.getcwd()) if 'help_' in x and '.dta' in x]
dfs = []
for f in files:
    print(f)
    dfs.append(df.merge(pd.read_stata(f)[keepvars], on = [idvar, tvar]))
df = pd.concat(dfs, ignore_index = True)

###############################################################################
# OUTPUT DATASET OF WHEN INDIVIDUALS REACH MAXIMUM DEBT BALANCES
###############################################################################

# Calculate and output age at which individuals have max real debt
df['real_debt'] = df['dindex_05'] * df['help_debt_bal']
df1 = df[df.groupby(idvar)['new_help'].transform(max) > 0].copy()
maxes = df1[df1.groupby(idvar)['real_debt'].transform(max) == df1['real_debt']].copy()
maxes = maxes.sort_values([idvar, tvar]).drop_duplicates(subset = [idvar], keep = 'first')
maxes = maxes[maxes[tvar] < 2019]
print(maxes['c_age_30_june'].describe(np.linspace(0,1)))
maxes = maxes[[idvar, tvar, 'c_age_30_june', 'real_debt', 'new_help']]
maxes = maxes.rename(columns = {tvar:'debtmax_year', 'c_age_30_june':'debtmax_age', 
                                'real_debt':'debt_max', 'new_help':'debtmax_new'})

# Plot debt balances around max debt year
df = df.merge(maxes, on = idvar, how = 'left')
df['max_time'] = df[tvar] - df['debtmax_year']
df.groupby('max_time')[['help_debt_bal', 'real_debt']].mean().plot()
df.groupby('max_time')[['help_debt_bal', 'real_debt']].count().plot()
df.groupby('max_time')['new_help'].mean()

# Calculate max, mean, and variance of log max debt conditional on being above
# a small amount and being below 26
debtmax = maxes.query('debtmax_age <= 26')['debt_max'].copy()
max_d = np.max(debtmax)
debtmax = np.log(debtmax[debtmax > 100])
mu_d, sigma_d = norm.fit(np.array(debtmax))

# Output in one file, reading in p_e from debt0 distribution
os.chdir(momdir)
p_e = np.genfromtxt('debt0_distribution.txt')[0]
series_txt(pd.Series([p_e, mu_d, sigma_d, max_d]), 'debtmax_distribution.txt')
