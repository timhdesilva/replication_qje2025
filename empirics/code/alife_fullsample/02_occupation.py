import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Imports
from directories import datadir, figdir, inputdir

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

# Read in hours and rte statistics
os.chdir(inputdir)
hours = pd.read_csv('sdloghours_occupation2.csv').dropna()
hours['c_occupation'] = hours['occupation2_code'].astype(float)

# Loop to stack data from all years
dfs = []
for yr in range(firstyr, lastyr + 1):

    # Read in data and filter
    os.chdir(datadir)
    data = pd.read_stata(f'help_{yr}.dta')
    data['year'] = data[tvar]
    data = data[data['help_has_debt'] == 1]
    data = data.query('has_trust == 0')
    data = data[(data['c_age_30_june'] >= min_age) & (data['c_age_30_june'] <= max_age)]
    
    # Adjust for inflation
    infl = data['tindex_05'].median()
    infl_vars = [incvar, 'threshold']
    data[infl_vars] *= infl

    # Variables of interest
    keeps = [idvar, 'year', incvar, 'c_occupation', 'entrep']

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
    
# Merge in hours and rte statistics
df = df.merge(hours, how = 'left', on  = 'c_occupation')

# Function to make datasets
def belowratio_heterogeneity(data_in, var, tsh, 
                             brange, catvar, xvar, labvar,
                             drop_entrep):
    data = data_in.copy()
    if drop_entrep:
        data = data.query('entrep == 0').copy()
    # Calculate number above and below
    data['below'] = ((data[var] <= tsh) & (data[var] >= tsh - brange)).astype(int)
    data['above'] = ((data[var] > tsh) & (data[var] <= tsh + brange)).astype(int)
    # Calculate ratio of above and below
    counts = data.groupby(catvar)[['below', 'above']].sum()
    counts['ratio'] = counts['below'] / counts['above']
    # Merge in total number of people in occupation
    occ_tot = data.groupby(catvar)[[var]].count().rename(columns = {var:'n_total'})
    counts = counts.merge(occ_tot, left_index = True, right_index = True)
    # Merge in xvariable for plot and labels
    labdf = data_in.groupby(catvar)[[xvar, labvar]].first()
    counts = counts.merge(labdf, left_index = True, right_index = True)
    return counts.dropna().sort_values('ratio').reset_index()

# Make datasets
br = 2500
entp = False
chg = belowratio_heterogeneity(df, incvar, tsh, br, 'c_occupation', 'chg', 'occupation2_label', entp)

# Read in stats and merge in bunching stats
os.chdir(inputdir)
stats = pd.read_csv('occupation2_stats.csv')
bunch_stats = stats.merge(chg[['c_occupation', 'occupation2_label', 'below', 'above', 'ratio', 'chg']], on = 'c_occupation')
bunch_stats['income_max_unscaled'] = bunch_stats['income_max']
bunch_stats['income_max'] /= bunch_stats['income_max'].mean()

# Add total occupation counts to others
chg = chg.merge(stats[['c_occupation', 'count']], on = 'c_occupation', how = 'left')

# Function for plotting
def scatter_oc(df, x, xlab, labshift = 0):
    fig, ax = plt.subplots(1,1)
    fig.set_size_inches([8,5])
    df.plot.scatter(x = x, y = 'ratio', ax = ax, color = 'darkblue', alpha = 0.7)
    b, se = np.polyfit(df[x], df['ratio'], 1, cov = True)
    mc, bc = b
    se = np.sqrt(np.diag(se))[0]
    xc = np.linspace(df[x].min(), df[x].max())
    ax.plot(xc, mc*xc + bc, color = 'black', alpha = 0.4, linestyle = '--')
    ax.annotate(f'Slope = {np.round(mc,2)} ({np.round(se,2)})', (ax.get_xlim()[1] - 0.12 + labshift, ax.get_ylim()[0]*1.02))
    ax.set_xlabel(xlab)
    ax.set_ylabel('Ratio of Debtholders Below to Above 0% Threshold')
    return fig, ax

# Make plot
fig, ax = scatter_oc(chg, 'chg', 'Hourly Flexibility: Within-Occupation SD of Changes in Log Hours')
oc_codes = [43, 62, 85, 26]
labs = {43:'   Hospitality and Restaurant Workers   ', 
        62:'   Sales Workers   ',
        85:'   Food Preparation Assistants  ',
        26:'   Information and\n   Technology Professionals   '}
for i, row in chg.iterrows():
    if row['c_occupation'] in oc_codes:
        ax.scatter(row['chg'], row['ratio'], color = 'white', alpha = 1) # block points
        ax.scatter(row['chg'], row['ratio'], color = 'maroon', alpha = 0.7) # plot in color
        if row['chg'] < np.median(chg['chg']):
            ha = 'left'
        else:
            ha = 'right'
        ax.annotate(labs[row['c_occupation']], (row['chg'], row['ratio']), color = 'maroon', # text
                    ha = ha, va = 'center', fontsize = 8)
os.chdir(figdir)
plt.savefig('occupationplot_slides_chg_label.pdf', bbox_inches = 'tight')

