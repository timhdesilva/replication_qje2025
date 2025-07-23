import os
import pandas as pd
import numpy as np

# Imports
from directories import taxdir_dta, tbdir
from fxns_globals import indexation_t, min_wage, clip_outlier

###############################################################################
# FILE PARAMETERS
###############################################################################

# Years of data to look at
firstyr = 1991
lastyr = 2019

# Age filter
min_age = 20
max_age = 64

# Unique identifier
idvar = 'alife_id_001'

###############################################################################
# MAKE POOLED DATASET
###############################################################################

# Stack datasets
dfs = []
for yr in range(firstyr, lastyr + 1):
    # Read in data
    os.chdir(taxdir_dta)
    data = pd.read_stata('aLife10_tax_' + str(yr) + '.dta')
    data['year'] = yr
    if 'c_occupation' in data.columns:
        # Filters
        data = data[(data['c_age_30_june'] >= min_age) & (data['c_age_30_june'] <= max_age)]
        data = data[data['c_self_emp_flag'] == 0]
        data = data[~data['isn_pt_trust_dist_npp_cd'].isin(['S', 'T', 'I'])]
        # Keep columns you need
        data['age'] = data['c_age_30_june']
        data = data[['c_occupation', 'age', 'i_salary_wage', 'i_allowances']]
        # Filter to those above 0.5x minimum wage x 13 full time(38hr) weeks: Guvenen et al. (2014) criteria (ii)
        data = data[data['i_salary_wage'] >= 0.5*38*13*min_wage[yr]]
        # Winsorize two income measures at 99.999 following Guvenen et al. (2014)
        data = clip_outlier(data, ['i_salary_wage', 'i_allowances'], p_low = 0.0, p_high = 0.99999)
        # Adjust for inflation
        level_05 = np.product([indexation_t[x] for x in range(1991,2006)])
        level_yr = np.product([indexation_t[x] for x in range(1991, yr + 1)])
        data[['i_salary_wage', 'i_allowances']] *= (level_05/level_yr)
        # Take out year fixed effects
        data['wage_fe'] = data['i_salary_wage'] - data['i_salary_wage'].mean()
        # Append, dropping missing
        dfs.append(data.dropna())
    del data
    print('Finished year:', yr)

# Concatenate
df = pd.concat(dfs, ignore_index = True)
del dfs

# Add back constant so you take out year fixed effects
df['wage_fe'] += (df['i_salary_wage'].mean())

###############################################################################
# CALCULATE AVERAGE FRACTION OF TIPS IN EACH OCCUPATION
###############################################################################

# Calculate ratio of allowances to salary
df['tip_ratio'] = df['i_allowances'] / df['i_salary_wage']

# Micro mean by occupation
micro = df.groupby('c_occupation')['tip_ratio'].mean()
micro = micro.reset_index().rename(columns = {'tip_ratio':'tips_micro'})

# Macro mean by occupation
num = df.groupby('c_occupation')['i_allowances'].sum()
den = df.groupby('c_occupation')['i_salary_wage'].sum()
macro = num / den
macro = macro.reset_index()
macro.columns = ['c_occupation', 'tips_macro']

# Fraction with positive allowances
df['pos_tips'] = (df['i_allowances'] > 0).astype(int)
pos = df.groupby('c_occupation')['pos_tips'].mean().reset_index()
    
# Combine into output dataframe
output = micro.merge(macro, on = 'c_occupation')
output = output.merge(pos, on = 'c_occupation')

###############################################################################
# ADD LIFE CYCLE INCOME STATISTICS
###############################################################################

# Income variable
incv = 'wage_fe'

# Counts by occupation
counts = df.groupby('c_occupation')[incv].count()
counts = counts.reset_index().rename(columns = {incv:'count'})

# Calculate means by age for each occupation
profiles = df.groupby(['c_occupation', 'age'])[incv].mean().reset_index()

# Maximum earnings in profile
maxes = profiles.groupby('c_occupation')[incv].max()
maxes = maxes.reset_index().rename(columns = {incv:'income_max'})

# Calculate initial earnings levels at 26
initials = profiles.query('age == 26').drop(['age'], axis = 1).rename(columns = {incv:'income_26'})

# Calculate earnings levels at 45, which is point in which pooled average hits max
mids = profiles.query('age == 45').drop(['age'], axis = 1).rename(columns = {incv:'income_45'})

# Combine into output dataframe
output = output.merge(counts, on = 'c_occupation')
output = output.merge(initials, on = 'c_occupation')
output = output.merge(maxes, on = 'c_occupation')
output = output.merge(mids, on = 'c_occupation')

# Calculate slope
output['income_slope'] = output['income_45']/output['income_26'] - 1

###############################################################################
# OUTPUT RESULTS
###############################################################################

os.chdir(tbdir)
output.to_csv('occupation2_stats.csv', index = False)
