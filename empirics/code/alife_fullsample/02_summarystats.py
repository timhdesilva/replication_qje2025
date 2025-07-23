import os
import numpy as np
import pandas as pd
from functools import reduce

# Imports
from directories import datadir, tbdir
from fxns_globals import indexation_t, clip_outlier

###############################################################################
# FILE PARAMETERS
###############################################################################

# Variables
idvar = 'alife_id_001' # unique individual identifier
tvar = 'income_year' # variable for income year

###############################################################################
# ANALYSIS SETTINGS
###############################################################################

# Years pre and post policy to do summary stats
firstyr = 1991
lastyr = 2019

# Year to deflate variables to
deflyr = 2005

# Variables to summarize based on 
sumvars_df = ['help_income', 'ic_taxable_income_loss', 'i_salary_wage', 'labor_income', 'interest_dividend',
           'capital_income', 'net_deduc', 'help_debt_bal', 'help_reqpay', 'sb_mem_bal', 'voluntary_super']
sumvars_b = ['c_age_30_june', 'female', 'entrep', 'below_min', 'below_tsh', 'new_help',
             'make_voluntary_super']
sumvars = sumvars_df + sumvars_b

# Age filter
min_age = 20
max_age = 64

###############################################################################
# MAKE DATASET
###############################################################################

# Read in data
dfs = []
p_defl = np.product([indexation_t[x] for x in range(1991,deflyr+1)])
for yr in range(firstyr, lastyr):
    df = pd.read_stata(f'{datadir}/help_{yr}.dta')
    df = df.query('c_age_30_june <= @max_age and c_age_30_june >= @min_age')
    df = df.query('has_trust == 0')
    if yr <= 2004:
        df['Post'] = 0
    else:
        df['Post'] = 1
    p_yr = np.product([indexation_t[x] for x in range(1991,yr+1)])
    for x in sumvars:
        if x not in df.columns:
            df[x] = np.nan
    df[sumvars_df] *= (p_defl / p_yr)
    dfs.append(df[[idvar, tvar, 'Post', 'help_has_debt'] + sumvars])
    print(f'\rFinished year = {yr}', end = '')
df = pd.concat(dfs, ignore_index = True)
del dfs

# Replace HELP payment to zero if no debt
if 'help_reqpay' in df.columns:
    df.loc[df['help_has_debt'] == 0, 'help_reqpay'] = 0

# Winsorize
no_w = ['help_debt_bal', 'help_reqpay']
df = clip_outlier(df, [x for x in sumvars_df if x not in no_w])
    
# Create dummy variable for whether people have made a payment yet
df = df.sort_values([idvar, tvar])
df['pay'] = (df['help_reqpay'] > 0).astype(int)
df['pays'] = df.groupby(idvar)['pay'].cumsum()
df['pay_never'] = (df['pays'] == 0).astype(int)
df = df.drop(['pay', 'pays'], axis = 1)

###############################################################################
# OUTPUT SUMMARY STATISTIC TABLES
###############################################################################

os.chdir(tbdir)

# Summary stats for all years
means = df.groupby(['help_has_debt'])[sumvars].mean()
counts = df.groupby(['help_has_debt'])[['help_income']].count().rename(columns = {'help_income':'N'})
alls = reduce(lambda x,y: pd.merge(x, y, left_index = True, right_index = True),
              [means, counts]).reset_index()
alls.to_csv(f'means.csv', index = False)

# Summary stats by time period before and after policy
means = df.query(f'{tvar} >= 1998 and {tvar} <= 2018').groupby(['Post', 'help_has_debt'])[sumvars].mean()
counts = (df.query(f'{tvar} >= 1998 and {tvar} <= 2018')
            .groupby(['Post', 'help_has_debt'])[['help_income']]
            .count().rename(columns = {'help_income':'N'})
        )
alls_by = reduce(lambda x,y: pd.merge(x, y, left_index = True, right_index = True),
              [means, counts]).reset_index()
alls_by.to_csv(f'means_prepost.csv', index = False)

# Summary stats at 26
means = df.query('c_age_30_june == 26').groupby(['help_has_debt'])[sumvars].mean()
counts = (df.query('c_age_30_june == 26')
            .groupby(['help_has_debt'])[['help_income']]
            .count().rename(columns = {'help_income':'N'})
        )
first = reduce(lambda x,y: pd.merge(x, y, left_index = True, right_index = True),
              [means, counts]).reset_index()
first.to_csv('means_age26.csv', index = False)
