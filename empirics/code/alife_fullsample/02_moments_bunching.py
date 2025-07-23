import os
import pandas as pd
import numpy as np

# Imports
from directories import datadir, momdir, sedir
from fxns_globals import indexation_t
from fxns_bootstrap import bootstrap_vector

###############################################################################
# FILE PARAMETERS
###############################################################################

# Important variables
idvar = 'alife_id_001' # unique individual identifier
tvar = 'income_year' # variable for income year

# Parameters
pre_period = [2002, 2003, 2004] # years in pre-period
pre_tshyr = 2004 # year to set threshold on in pre-period
pre_range = [22500, 28500] # range in pre-period in rounded pre_tshyr dollars
post_period = [2005, 2006, 2007] # years in post-period
post_tshyr = 2005 # year to set threshold on in post-period
post_range = [32500, 38500] # range in post-period in rounded post_tshyr dollars
binw = 500 # width of histogram bins
inc_var = 'help_income' # income variable for bunching
min_age = 23 # minimum age to be considered
max_age = 64 # maximum age to be considered
min_cohort = 1963 # minimum cohort to be considered
Nb = 1000 # number of bootstraps

###############################################################################
# MAKE DATASET OF INFORMATION
###############################################################################

# Calculate threshold and rounded threshold in pre period
os.chdir(datadir)
data = pd.read_stata("help_" + str(pre_tshyr) + ".dta")
pre_tsh = data['threshold'].median()
pre_tsh_r = binw * np.round(np.floor(pre_tsh / binw))

# Calculate threshold and rounded threshold in post period
data = pd.read_stata("help_" + str(post_tshyr) + ".dta")
post_tsh = data['threshold'].median()
post_tsh_r = binw * np.round(np.floor(post_tsh / binw))

# Price level in pre_tshyr and post_tshyr
pre_infl = np.product([indexation_t[x] for x in range(1991, pre_tshyr + 1)])
post_infl = np.product([indexation_t[x] for x in range(1991, post_tshyr + 1)])

# Make pre_df dataset
pre_df = []
for yr in pre_period:
    # Read in data and filter
    os.chdir(datadir)
    data = pd.read_stata("help_" + str(yr) + ".dta")
    data = data[data['help_has_debt'] == 1]
    data = data.query('has_trust == 0')
    data = data[(data['c_age_30_june'] >= min_age) & (data['c_age_30_june'] <= max_age)]
    data['cohort'] = (data[tvar] - data['c_age_30_june'] + 22).astype(int)
    data = data.query('cohort >= @min_cohort')
    # Adjust for inflation
    data['Y'] = data[inc_var] * pre_infl / np.product([indexation_t[x] for x in range(1991, yr + 1)])
    data['help_debt_bal'] *= (post_infl / np.product([indexation_t[x] for x in range(1991, yr + 1)]))
    # Center income around threshold, subtracting 1 so people at 0 are first non-bunchers
    data['Y'] = binw * np.floor((data['Y'] - pre_tsh - 1) / binw) + pre_tsh_r
    # Filter people close to the threshold based on rounded income
    data = data[(data['Y'] >= pre_range[0] - binw) & (data['Y'] <= pre_range[1] + binw)]
    # Stack in dataset
    pre_df.append(data[[idvar, tvar, 'Y', 'c_age_30_june', 'help_debt_bal']])
pre_df = pd.concat(pre_df, ignore_index = True)

# Make post_df dataset
post_df = []
for yr in post_period:
    # Read in data and filter
    os.chdir(datadir)
    data = pd.read_stata("help_" + str(yr) + ".dta")
    data = data[data['help_has_debt'] == 1]
    data = data.query('has_trust == 0')
    data = data[(data['c_age_30_june'] >= min_age) & (data['c_age_30_june'] <= max_age)]
    data['cohort'] = (data[tvar] - data['c_age_30_june'] + 22).astype(int)
    data = data.query('cohort >= @min_cohort')
    # Adjust for inflation
    data['Y'] = data[inc_var] * post_infl / np.product([indexation_t[x] for x in range(1991, yr + 1)])
    data['help_debt_bal'] *= (post_infl / np.product([indexation_t[x] for x in range(1991, yr + 1)]))
    # Center income around threshold, subtracting 1 so people at 0 are first non-bunchers
    data['Y'] = binw * np.floor((data['Y'] - post_tsh - 1) / binw) + post_tsh_r
    # Filter people close to the threshold
    data = data[(data['Y'] >= post_range[0] - binw) & (data['Y'] <= post_range[1] + binw)]
    # Stack in dataset
    post_df.append(data[[idvar, tvar, 'Y', 'c_age_30_june', 'help_debt_bal']])
post_df = pd.concat(post_df, ignore_index = True)
    
###############################################################################
# MAIN SCRIPT
###############################################################################

# Function to calculate income distribution
def calc_distribution(qry, df_in):
    # Sample selection
    if qry == '':
        df = df_in.copy()
    else:
        df = df_in.query(qry).copy()
    # Calculate bunching percentages
    bunch = df.groupby('Y')[idvar].count()
    bunch = bunch.iloc[1:-1] # drop first and last bins
    bunch = bunch / bunch.sum() * 100
    # Format
    bunch = bunch.reset_index()
    bunch['Y'] = bunch['Y'].astype(int)
    return bunch, np.array(bunch[idvar]).reshape(-1)

# Function to output bunching based on query for sample selection
def output_bunch(qry, name):
    # Calculate distributions and output to .csv files
    pre_bunch = calc_distribution(qry, pre_df)[0]
    post_bunch = calc_distribution(qry, post_df)[0]
    os.chdir(momdir)
    pre_bunch.round(8).to_csv(f'bunching_pre{name}.csv', header = None, index = None, sep = '\t')
    post_bunch.round(8).to_csv(f'bunching_post{name}.csv', header = None, index = None, sep = '\t')
    # Bootstrap and write to .csvs
    se_pre = bootstrap_vector(pre_df, lambda x: calc_distribution(qry, x)[1], Nb)
    se_post = bootstrap_vector(post_df, lambda x: calc_distribution(qry, x)[1], Nb)
    os.chdir(sedir)
    pd.Series(se_pre).round(8).to_csv(f'bunching_pre{name}.csv', header = None, index = None, sep = '\t')
    pd.Series(se_post).round(8).to_csv(f'bunching_post{name}.csv', header = None, index = None, sep = '\t')
    
# Run
output_bunch('', '')
