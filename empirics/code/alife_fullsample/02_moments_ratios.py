import os
import pandas as pd
import numpy as np

# Imports
from directories import datadir, momdir, sedir
from fxns_globals import indexation_t, series_txt
from fxns_bootstrap import bootstrap_statistic

###############################################################################
# FILE PARAMETERS
###############################################################################

# Important variables
idvar = 'alife_id_001' # unique individual identifier
tvar = 'income_year' # variable for income year

# Parameters
inc_var = 'help_income' # income variable for bunching
min_age = 23 # minimum age to be considered
max_age = 64 # maximum age to be considered
min_cohort = 1963 # minimum cohort to be considered
Nb = 1000 # number of bootstraps

###############################################################################
# MAIN SCRIPT
###############################################################################

# Function to adjust for inflation to 2005 dollars using HELP threshold indexation
def adjust_infl(y, year):
    level_05 = np.product([indexation_t[x] for x in range(1991,2006)])
    level_yr = np.product([indexation_t[x] for x in range(1991, year + 1)])
    return y * level_05 / level_yr

# Function to make dataset
def make_df(yrs, tsh, binw):
    dfs = []
    for yr in yrs:
        # Read in data and filter
        os.chdir(datadir)
        data = pd.read_stata("help_" + str(yr) + ".dta")
        data = data[data['help_has_debt'] == 1]
        data = data.query('has_trust == 0')
        data = data[(data['c_age_30_june'] >= min_age) & (data['c_age_30_june'] <= max_age)]
        data['cohort'] = (data[tvar] - data['c_age_30_june'] + 22).astype(int)
        data = data.query('cohort >= @min_cohort')
        # Adjust for inflation
        data['Y'] = data[inc_var].apply(adjust_infl, args = (yr,))
        data['help_debt_bal'] = data['help_debt_bal'].apply(adjust_infl, args = (yr,))
        # Stack in dataset
        dfs.append(data[[idvar, tvar, 'Y', 'c_age_30_june', 'help_debt_bal']])
        print(yr)
    # Concatnenate and calculate variables of interest
    df = pd.concat(dfs, ignore_index = True)
    df['below'] = ((df['Y'] > tsh - binw) & (df['Y'] <= tsh)).astype(int)
    df['above'] = ((df['Y'] > tsh) & (df['Y'] <= tsh + binw)).astype(int)
    df['debtQ'] = pd.qcut(df['help_debt_bal'], 4, labels = range(1,5))
    return df

# Function to output bunching based on query for sample selection
def calc_bunch(qry, df_in):
    # Sample selection
    if qry == '':
        df = df_in.copy()
    else:
        df = df_in.query(qry).copy()
    # Calculate bunch ratio
    below = df['below'].mean()
    above = df['above'].mean()
    return below/above
    
# Make datasets
df04 = make_df(list(range(1998,2005)), adjust_infl(25347, 2004), 500)
df05 = make_df(list(range(2005,2019)), 35000, 500)
df05_1 = make_df(list(range(2005,2019)), 38987, 500)

# Calculate ratios
ratios = [
        calc_bunch('', df04),
        calc_bunch('', df05),
        calc_bunch('', df05_1),
        calc_bunch('debtQ == 1', df05),
        calc_bunch('debtQ == 4', df05),
]
ratios.append(ratios[4] / ratios[3])
os.chdir(momdir)
series_txt(pd.Series(ratios[:3]), 'ratios_pooled_500.txt')
series_txt(pd.Series(ratios[3:]), 'ratios_pooled_bydebt_500.txt')

# Calculate standard errors
se_ratios = [
        bootstrap_statistic(df04, lambda x: calc_bunch('', x), Nb),
        bootstrap_statistic(df05, lambda x: calc_bunch('', x), Nb),
        bootstrap_statistic(df05_1, lambda x: calc_bunch('', x), Nb),
        bootstrap_statistic(df05, lambda x: calc_bunch('debtQ == 1', x), Nb),
        bootstrap_statistic(df05, lambda x: calc_bunch('debtQ == 4', x), Nb),
        bootstrap_statistic(df05, lambda x: calc_bunch('debtQ == 4', x) / calc_bunch('debtQ == 1', x), Nb),
]
os.chdir(sedir)
series_txt(pd.Series(se_ratios[:3]), 'ratios_pooled_500.txt')
series_txt(pd.Series(se_ratios[3:]), 'ratios_pooled_bydebt_500.txt')
