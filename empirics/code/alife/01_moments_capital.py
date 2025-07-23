import os
import pandas as pd
import numpy as np

# Imports
from directories import taxdir_dta, datadir, momdir, file_debt
from fxns_globals import clip_outlier, series_txt, indexation_t

###############################################################################
# FILE PARAMETERS
###############################################################################

# Years of data to look at
firstyr = 1991
lastyr = 2019

# Age filter and minimum cohort
min_age = 22
max_age = 64
min_cohort = 1963

# Variables to include in capital income
capitalvars = ['i_annuities_txd', 'i_annuities_untxd',
               'i_annuities_lsum_txd', 'i_annuities_lsum_untxd',
               'i_super_lsum_txd', 'i_super_lsum_untxd',
               'i_interest', 'i_div_frank', 'i_div_unfrank',
               'trustNPP_b13', 'is_cg_net', 'is_net_rent']

# Unique identifier
idvar = 'alife_id_001'

# Age bin size
bin_a = 5

# Output file name
filename = 'capitalpanel_' + str(firstyr) + '_' + str(lastyr) + '.dta'

###############################################################################
# MAKE POOLED DATASET OF INCOME
###############################################################################

# Function to adjust for inflation to 2005 dollars using HELP threshold indexation
def adjust_infl(y, year):
    level_05 = np.product([indexation_t[x] for x in range(1991,2006)])
    level_yr = np.product([indexation_t[x] for x in range(1991, year + 1)])
    return y * level_05 / level_yr

# Stack datasets
dfs = []
for yr in range(firstyr, lastyr + 1):
    # Read in data
    os.chdir(taxdir_dta)
    data = pd.read_stata('aLife10_tax_' + str(yr) + '.dta')
    data['year'] = yr
    # Filters
    data = data[(data['c_age_30_june'] >= min_age) & (data['c_age_30_june'] <= max_age)]
    data = data[data['c_self_emp_flag'] == 0]
    if 'isn_pt_trust_dist_npp_cd' in data.columns:
        data = data[~data['isn_pt_trust_dist_npp_cd'].isin(['S', 'T', 'I'])]
    # Create capital income variable
    data['capital_income'] = 0
    for v in capitalvars:
        if v in data.columns:
            data['capital_income'] += data[v].fillna(0)
    # Winsorize income at 99.999
    data = clip_outlier(data, 'capital_income', p_low = 0.0, p_high = 0.99999)
    # Adjust for inflation
    data['capital_income'] = data['capital_income'].apply(adjust_infl, args = (yr,))
    # Keep only columns that you need
    data['age'] = data['c_age_30_june']
    data = data[[idvar, 'year', 'capital_income', 'age']].dropna()
    # Append
    dfs.append(data)
    del data
    print('Finished year:', yr)

# Concatenate
df = pd.concat(dfs, ignore_index = True).sort_values([idvar, 'year']).reset_index()
del dfs

# Read in debt balances
debtbals = pd.read_csv(file_debt)
debtbals[idvar] = debtbals[idvar].astype(float) # convert to format of aLife10_tax files
debtbals = debtbals.rename(columns = {'help_debt_bal':'debt_balance', 'income_year':'year'})
debtbals = debtbals[(debtbals['year'] >= firstyr) & (debtbals['year'] <= lastyr)]
# Trim few # of negative balance observations
debtbals['debt_balance'] = np.maximum(debtbals['debt_balance'], 0)
# Adjust for inflation
debtbals['debt_balance'] = debtbals.apply(
        lambda x: adjust_infl(x['debt_balance'], int(x['year'])),
        axis = 1
)
# Merge and fill missing values with zero
df = df.merge(debtbals, on = [idvar, 'year'], how = 'left')
df['debt_balance'] = df['debt_balance'].fillna(0)     
# Calculate positive debt balance dummy
df['has_debt'] = (df['debt_balance'] > 0).astype(int)

# Merge in whether people had debt at minimum age
debt_min = df[df['age'] == min_age][[idvar, 'debt_balance']].copy()
debt_min = debt_min.rename(columns = {'debt_balance':'debt_initial'})
debt_min['has_debt_initial'] = (debt_min['debt_initial'] > 0).astype(int)
df = df.merge(debt_min, on = idvar, how = 'left')
df[['debt_initial', 'has_debt_initial']] = df[['debt_initial', 'has_debt_initial']].fillna(0)

# Output to Stata
os.chdir(datadir)
df.to_stata(filename, write_index = False)

###############################################################################
# CALCULATE CAPITAL INCOME AGE PROFILE
###############################################################################

# Calculate cohort and filter
df['cohort'] = (df['year'] - df['age'] + min_age).astype(int)
df = df.query('cohort >= @min_cohort')

# Output directory
os.chdir(momdir)

# Plot raw data
df.groupby('age')['capital_income'].mean().plot()
print(df.groupby('age')['capital_income'].mean())

# Round age and make cohort bins
df['age'] = np.floor(df['age'] / bin_a) * bin_a
df['cohort'] = df['year'] - df['age']
df[['age', 'year', 'cohort']] = df[['age', 'year', 'cohort']].astype(int)

# Estimate raw age effects and output
age_effects = df.groupby('age')['capital_income'].mean().sort_index()
series_txt(age_effects, 'ageprofile_raw_capital.txt')
