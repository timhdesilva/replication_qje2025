import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Imports
from directories import datadir, figdir
from fxns_bunching import bstat_heterogeneity

###############################################################################
# FILE PARAMETERS
###############################################################################

# Variables
idvar = 'alife_id_001' # unique individual identifier
tvar = 'income_year' # variable for income year

# Income variable
incvar = 'help_income'

# Sample filters
firstyr = 2005
lastyr = 2018
min_age = 20
max_age = 64

# Parameters for plots
binw = 250
bins_below = int(2500/binw)
method = 'spline'
dof = 4
Nb = 0

###############################################################################
# COLLECT AND FORMAT DATA
###############################################################################

# Loop to stack data from all years
dfs = []
for yr in range(firstyr, lastyr + 1):

    # Read in data and filter
    os.chdir(datadir)
    data = pd.read_stata(f'help_{yr}.dta')
    data['year'] = data[tvar]
    data = data[(data['help_has_debt'] == 1) & (data['has_trust'] == 0)]
    data = data[(data['c_age_30_june'] >= min_age) & (data['c_age_30_june'] <= max_age)]

    # Create variables
    data['debtQ'] = pd.qcut(data['help_debt_bal'], 4, labels = range(1, 5))
    data['superQ'] = pd.qcut(data['sb_mem_bal'], 4, labels = range(1, 5))
    def age_bins(x):
        if x < 25:
            return 25
        elif x < 30:
            return 30
        elif x < 35:
            return 35
        elif x < 40:
            return 40
        else:
            return 45
    data['age_bin'] = data['c_age_30_june'].apply(age_bins)
    data['meddebt_age'] = data.groupby('age_bin')['help_debt_bal'].transform('median')
    data['debtQ2_age'] = (data['help_debt_bal'] > data['meddebt_age']).astype(int)
    
    # Adjust for inflation
    infl = data['tindex_05'].median()
    infl_vars = [incvar, 'threshold']
    data[infl_vars] *= infl

    # Variables of interest
    keeps = [idvar, 'year', incvar, 'c_age_30_june', 'c_occupation', 'age_bin', 'debtQ', 'debtQ2_age', 'superQ']

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

# Sort
df = df.sort_values(['id', 'year'])

###############################################################################
# MAIN SCRIPT
###############################################################################

# Change directory
os.chdir(figdir)

# Range for heterogeneity plots
lowY = 30000
highY = 40000

# Age and debt split: debt by age
age_vals = list(np.sort(df['age_bin'].unique()))
belowD = bstat_heterogeneity(df.query('debtQ2_age == 0'), idvar, incvar, tsh, lowY, highY, binw,
                             'age_bin', age_vals, bins_below, method, dof)
aboveD = bstat_heterogeneity(df.query('debtQ2_age == 1'), idvar, incvar, tsh, lowY, highY, binw,
                             'age_bin', age_vals, bins_below, method, dof)
ages = ['20-24', '25-29', '30-34', '35-39', '40+']
debts = ['Below Median HELP Debt', 'Above Median HELP Debt']
pdf_d = pd.DataFrame({'Debt':np.repeat(debts, len(ages)), 'Age':np.tile(ages, 2), 'b':belowD + aboveD})
fig, ax = plt.subplots(figsize = (8, 5))    
pdf_d.query(f'Debt == "{debts[0]}"')['b'].plot(ax = ax, label = debts[0], color = 'darkblue', kind = 'bar', 
                                            alpha = 0.7, rot = 0, align = 'center', width = 0.4, position = 1)
pdf_d.query(f'Debt == "{debts[1]}"')['b'].plot(ax = ax, label = debts[1], color = 'maroon', kind = 'bar', 
                                            alpha = 0.3, rot = 0, align = 'center', width = 0.4, position = 0)
ax.set_xticklabels(ages)
ax.set_xlabel('Age')
ax.set_ylabel('Excess Bunching Mass: $b$')
ax.legend()
ax.set_xlim((-.5, ax.get_xlim()[1] - 0.15))
plt.savefig('bstat_agedebt.pdf', bbox_inches = 'tight')

# Super balances
s_vals = list(range(1,5))
supers = bstat_heterogeneity(df, idvar, incvar, tsh, lowY, highY, binw,
                             'superQ', s_vals, bins_below, method, dof)
Qs = ['Q1', 'Q2', 'Q3', 'Q4']
fig, ax = plt.subplots(figsize = (8*0.7, 5*0.8))
ax.bar(Qs, supers, color = 'darkblue', alpha = 0.7)
ax.set_xlabel('Quartile of Superannuation Balance')
ax.set_ylabel('Excess Bunching Mass: $b$')
plt.savefig('bstat_super.pdf', bbox_inches = 'tight')
