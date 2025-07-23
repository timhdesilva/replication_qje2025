import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Imports
from directories import datadir, figdir
from fxns_globals import indexation_t

###############################################################################
# FILE PARAMETERS
###############################################################################

# Variables
idvar = 'alife_id_001' # unique individual identifier
tvar = 'income_year' # variable for income year

# Paramaters for bunching
chgyr = 2005 # year to deflate income to (first yr after policy change)
firstyr = 2002 # first year to look at
lastyr = 2008 # last year to look at
colors = ['darkgray', 'darkviolet', 'darkblue', 'maroon', 'darkgreen', 'darkorange', 'black'] # must match firstyr - lastyr + 1 length
brange = 8000 # range around thresholds to look for bunching
binw = 500 # width of histogram bins
inc_var = 'help_income' # income variable for bunching
min_age = 20
max_age = 64

###############################################################################
# CALCULATE THRESHOLD IN BASE YEAR AND INCOME RANGES
###############################################################################

# Function to adjust for inflation to chgyr dollars using HELP threshold indexation
def adjust_infl(y, year):
    level_05 = np.product([indexation_t[x] for x in range(1991, chgyr + 1)])
    level_yr = np.product([indexation_t[x] for x in range(1991, year + 1)])
    return y * level_05 / level_yr

# Calculate threshold and rounded threshold in chgyr 
os.chdir(datadir)
data = pd.read_stata("help_" + str(chgyr) + ".dta")
tsh_post = data['threshold'].median()
tsh_post_r = binw * np.round(np.floor(tsh_post / binw))

# Calculate threshold and rounded threshold in chgyr-1
os.chdir(datadir)
data = pd.read_stata("help_" + str(chgyr - 1) + ".dta")
tsh_pre = adjust_infl(data['threshold'].median(), chgyr - 1)
tsh_pre_r = binw * np.round(np.floor(tsh_pre / binw))

# Calculate ranges
lowY = tsh_pre - brange
highY = tsh_post + brange

###############################################################################
# MAIN SCRIPT
###############################################################################

# Loop through years to make dataframe needed for lists
f_list = []
for yr in range(firstyr, lastyr + 1):

    # Read in data and filter
    os.chdir(datadir)
    data = pd.read_stata("help_" + str(yr) + ".dta")
    data = data.query('has_trust == 0')
    data = data[(data['c_age_30_june'] >= min_age) & (data['c_age_30_june'] <= max_age)]

    # Center income around threshold (subtract an additional 1 so that people at 0 are first non-bunchers)
    data['Y'] = data[inc_var].apply(adjust_infl, args = (yr,))
    if yr < chgyr:
        tsh_yr = tsh_pre
        tsh_yr_r = tsh_pre_r
    else:
        tsh_yr = tsh_post
        tsh_yr_r = tsh_post_r
    data['Y'] = data['Y'] - tsh_yr - 1
    data['Y'] = binw * np.round(np.floor(data['Y'] / binw)) + tsh_yr_r

    # Filter to people close to the treshold
    data = data[(data['Y'] >= lowY) & (data['Y'] <= highY)]

    # Keep variables of interest
    data = data[[idvar, 'help_debt_bal', 'Y']]

    # Calculate counts and percentages by groups
    f_nd = data[data['help_debt_bal'] == 0].groupby('Y')['alife_id_001'].count()
    f_d = data[data['help_debt_bal'] > 0].groupby('Y')['alife_id_001'].count()
    print('Minimum bin size for non-debt:', f_nd.min())
    print('Minimum bin size for debt:', f_d.min())
    f_nd = f_nd / f_nd.sum() * 100
    f_d = f_d / f_d.sum() * 100
    f = pd.DataFrame({'No Debt':f_nd, 'Debt':f_d}).reset_index()
    f_list.append(f)

# Loop to make plot for just first two years
Ys = ['No Debt', 'Debt']
ymax = np.max([f_list[i][Ys].max().max() for i in range(len(f_list))])*1.1
ymin = np.min([f_list[i][Ys].min().min() for i in range(len(f_list))])*0.9
styles = ['solid', 'dashed']
# Initialize plot
fig, axes = plt.subplots(1,2)
fig.set_size_inches(15, 4)
plt.subplots_adjust(wspace = 0.3)
axes[0].set_ylim([ymin, ymax])
axes[1].set_ylim([ymin, ymax])
axes[0].set_title('Individuals without Debt')
axes[1].set_title('Individuals with Debt')
# Make plot
for yr1 in range(chgyr - 1, chgyr + 1):
    f = f_list[yr1 - firstyr]
    if yr1 < chgyr:
        tsh_yr_r = tsh_pre_r
    else:
        tsh_yr_r = tsh_post_r
    for i in range(len(Ys)):
        f.plot(ax = axes[i], x = 'Y', y = Ys[i], kind = 'line', marker = 'o', linestyle = styles[yr1 - chgyr - 1],
         color = colors[yr1 - firstyr], label = str(yr1), alpha = 0.5)
        axes[i].axvline(tsh_yr_r, color = colors[yr1 - firstyr], 
            linestyle = styles[yr1 - chgyr - 1], alpha = 0.5)
        if i == 0:
            axes[i].text(tsh_yr_r - 750, 0.36*(ymax+ymin), f'{yr1} Threshold', 
                rotation = 90, fontsize = 7, color = colors[yr1 - firstyr])
# Format labels
for i in range(2):
    axes[i].legend()
    axes[i].set_xlabel('Income that Determines Student Debt Repayment')
    axes[i].set_ylabel('Percent of Individuals')
    axes[i].set_xticklabels(['{:,}'.format(int(x)) for x in axes[i].get_xticks()])
    axes[i].set_yticklabels(['{:,.2%}'.format(x) for x in axes[i].get_yticks()/100])
# Save plot
os.chdir(figdir)
plt.savefig('figure1_real.pdf', bbox_inches = 'tight')
plt.close()

# Loop to make plots separately for each year - debtholders only
ymax = np.max([f_list[i][Ys].max().max() for i in range(len(f_list))])*1.1
ymin = np.min([f_list[i][Ys].min().min() for i in range(len(f_list))])*0.9
for yr in range(firstyr, lastyr + 1):
    # Make plot separately for each year
    fig, axes = plt.subplots(1,1)
    fig.set_size_inches(7.5, 4)
    axes.set_ylim([ymin, ymax])
    f = f_list[yr - firstyr]
    alpha_p = 0.9
    if yr < chgyr:
        color_p = 'darkblue'
    else:
        color_p = 'maroon'
    f.plot(ax = axes, x = 'Y', y = 'Debt', kind = 'line', marker = 'o',
           color = color_p, label = str(yr), alpha = alpha_p)
    axes.axvline(tsh_pre_r, color = 'darkblue', alpha = alpha_p)
    axes.axvline(tsh_post_r, color = 'maroon', alpha = alpha_p)
    axes.set_xticklabels(['{:,}'.format(int(x)) for x in axes.get_xticks()])
    axes.set_yticklabels(['{:,.2%}'.format(x) for x in axes.get_yticks()/100])
    axes.legend()
    # Format lables
    axes.set_xlabel('Real HELP Income')
    axes.set_ylabel('Percent of Individuals')
    # Save plot
    os.chdir(figdir)
    plt.savefig('bunching_fixed_real_' + str(chgyr) + '_' + str(firstyr) + '_' + str(yr) + '_' + str(lastyr) + '_separate_debtonly.pdf',
            bbox_inches = 'tight')
    plt.close()
