import os
import pandas as pd
import numpy as np

# Imports
from directories import datadir
from fxns_bunching import binscatter_bunch

###############################################################################
# FILE PARAMETERS
###############################################################################

# Variables
idvar = 'SYNTHETIC_AEUID'
incvar = 'income_help'

# Sample filters
firstyr = 2016
lastyr = 2016
min_age = 20
max_age = 64

# Census 2016 variables to include
c16vars = ['HRSP', 'MRED', 'RNTD']

# Parameters for bunching plots
binw = 250

###############################################################################
# COLLECT AND FORMAT DATA
###############################################################################

# Loop to stack data from all years
dfs = []
for yr in range(firstyr, lastyr + 1):

    # Read in data and filter
    os.chdir(datadir)
    data = pd.read_stata("help_" + str(yr) + ".dta")
    if yr <= 2006 or yr >= 2011:
        data = data[(data['medicare_exempt'] == 0)]
    if yr <= 2010:
        data = data[data['resident'] == 'N']
    else:
        data = data[data['taxable'] == 'taxable']
    data = data[data['has_debt'] == 1]
    data = data[(data['age'] >= min_age) & (data['age'] <= max_age)]

    # Adjust for inflation
    infl = data['tindex_05'].median()
    infl_vars = [incvar, 'labor', 'wage_b1', 'threshold']
    data[infl_vars] *= infl

    # Variables of interest
    keeps = [idvar, 'year', incvar, 'labor', 'wage_b1', 'age']

    # Format Census 2016 data if requested
    replace_vals = {'&&&&':'', '@@@@':'', 'VV':'', '@@':'', '&&':'', '&':'', '@':''}
    data[c16vars] = data[c16vars].replace(replace_vals).apply(pd.to_numeric)
    # Annualize payments and adjust for inflation
    if 'MRED' in c16vars:
        data['MRED'] = 12 * data['MRED'] * data['tindex_05']
    if 'RNTD' in c16vars:
        data['RNTD'] = 52 * data['RNTD'] * data['tindex_05']
    if 'MRED' in c16vars and 'RNTD' in c16vars:
        data['house_total'] = data[['MRED', 'RNTD']].sum(axis = 1)
        data['house_total'] = data.apply( # set missing if rent AND mortgage missing
                lambda x: np.nan if np.isnan(x['MRED']) and np.isnan(x['RNTD'])
                    else x['house_total'],
                axis = 1
            )
        c16vars += ['house_total']
    # Add Census variables to keeps list
    keeps = keeps + c16vars + ['match_quality']

    # Calculate threshold in first year
    if yr == firstyr:
        tsh = int(data['threshold'].median())

    # Append dataset with variables of interest
    dfs.append(data[keeps])
    del data
    print(yr)

# Stack data
df = pd.concat(dfs, ignore_index = True)

# Adjust ID to be unique if multiple years and keep only id
df['id'] = df[idvar].copy()
if lastyr > firstyr:
    df[idvar] = df[idvar].astype(str) + df['year'].astype(str)

# Calculate housing payment to income ratio
df['house_pti'] = (df['house_total'] / df[incvar]).replace([np.inf, -np.inf], np.nan)

###############################################################################
# MAIN SCRIPT
###############################################################################

# Range for plots
lowY = 30000
highY = 40000

## Hours worked
binscatter_bunch(df, 2016, 2016, idvar, incvar, 'HRSP', 'Weekly Hours Worked', 
                 tsh, lowY, highY, binw, ytype = 'float', name = 'full')

## Mortgage and rent payment-to-income ratio
binscatter_bunch(df, 2016, 2016, idvar, incvar, 'house_pti', 'Housing Payment-to-Income Ratio', 
                 tsh, lowY, highY, binw, ytype = 'float', name = 'full')
