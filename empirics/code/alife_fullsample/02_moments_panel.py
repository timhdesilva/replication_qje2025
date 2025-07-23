import os
import pandas as pd
import numpy as np

# Imports
from directories import datadir, momdir, sedir
from fxns_globals import indexation_t, series_txt

###############################################################################
# FILE PARAMETERS
###############################################################################

# Important variables
idvar = 'alife_id_001' # unique individual identifier

# Parameters
inc_var = 'help_income' # income variable for bunching
min_age = 23 # minimum age to be considered
max_age = 64 # maximum age to be considered
binw = 500

###############################################################################
# MAIN SCRIPT
###############################################################################

# Function to adjust for inflation to 2005 dollars using HELP threshold indexation
def adjust_infl(y, year):
    level_05 = np.product([indexation_t[x] for x in range(1991,2006)])
    level_yr = np.product([indexation_t[x] for x in range(1991, year + 1)])
    return y * level_05 / level_yr

# Sample selection
smp = "help_has_debt == 1 and c_age_30_june >= @min_age and c_age_30_june <= @max_age and has_trust == 0"

# Set directory
os.chdir(datadir)

# Construct price adjustment from 2005 to 2004
price04 = 1. / (adjust_infl(1., 2004))

# Read in sample of individuals who were bunching in 2004
ids04 = pd.read_stata('help_2004.dta')
ids04 = ids04.query(smp)
ids04['bunch04'] = ((ids04[inc_var] > 25347 - binw) & (ids04[inc_var] <= 25347)).astype(int)
ids04['bunch_new'] = ((ids04[inc_var] > 35000*price04 - binw) & (ids04[inc_var] <= 35000*price04)).astype(int)
ids04['Y04'] = ids04[inc_var]
ids04 = ids04[[idvar, 'bunch04', 'bunch_new', 'Y04']]

# Read in sample of individuals who were bunching in 2005
ids05 = pd.read_stata('help_2005.dta')
ids05 = ids05.query(smp)
ids05['bunch05'] = ((ids05[inc_var] > 35000 - binw) & (ids05[inc_var] <= 35000)).astype(int)
ids05['bunch_old'] = ((ids05[inc_var]*price04 > 25347 - binw) & (ids05[inc_var]*price04 <= 25347)).astype(int)
ids05['Y05'] = ids05[inc_var]
ids05 = ids05[[idvar, 'bunch05', 'bunch_old', 'Y05']]

# Merge dataset of people in 2004 and 2005 with bunching indicators
ids = ids04.merge(ids05, on = idvar)

# Make dataset linking bunching at t with bunching at t+1, post 2005
dfs = []
for yr in range(2005,2018):
    df_t = pd.read_stata(f'help_{yr}.dta').query(smp)
    df_t[inc_var] *= adjust_infl(1., yr)
    df_t['bunch'] = ((df_t[inc_var] > 35000 - binw) & (df_t[inc_var] <= 35000)).astype(int)
    df_t = df_t.query('bunch == 1')[[idvar]]
    df_t1 = pd.read_stata(f'help_{yr + 1}.dta').query(smp)
    df_t1['help_income_t1'] = df_t1[inc_var] * adjust_infl(1., yr + 1)
    df_t = df_t.merge(df_t1[[idvar, 'help_income_t1']], on = idvar)
    df_t['bunch_t1'] = ((df_t['help_income_t1'] > 35000 - binw) & (df_t['help_income_t1'] <= 35000)).astype(int)
    dfs.append(df_t)
df = pd.concat(dfs, ignore_index = True)

# Calculate moments and SEs around 2004-2005
moms = [
    ids.query('bunch04 == 1')['bunch05'].mean(),
    ids.query('bunch04 == 1')['bunch_old'].mean(),
    ids.query('bunch05 == 1')['bunch04'].mean(),
    ids.query('bunch05 == 1')['bunch_new'].mean(),  
    df['bunch_t1'].mean(),
]
ses = [
    ids.query('bunch04 == 1')['bunch05'].sem(),
    ids.query('bunch04 == 1')['bunch_old'].sem(),
    ids.query('bunch05 == 1')['bunch04'].sem(),
    ids.query('bunch05 == 1')['bunch_new'].sem(),
    df['bunch_t1'].sem(),
]

# Output results
os.chdir(momdir)
series_txt(pd.Series(moms), f'panel_moments_{binw}.txt')
os.chdir(sedir)
series_txt(pd.Series(ses), f'panel_moments_{binw}.txt')

# Form and output second set of results
moms = [
    ids.query('bunch04 == 1')['Y05'].mean(),
    ids.query('bunch05 == 1')['Y04'].mean(),
]
ses = [
    ids.query('bunch04 == 1')['Y05'].sem(),
    ids.query('bunch05 == 1')['Y04'].sem(),
]
os.chdir(momdir)
series_txt(pd.Series(moms), f'panel_moments_Y_{binw}.txt')
os.chdir(sedir)
series_txt(pd.Series(ses), f'panel_moments_Y_{binw}.txt')
