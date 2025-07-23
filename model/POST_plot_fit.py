import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

###############################################################################
# ORGANIZE RESULTS
###############################################################################

# Set columns
params = ['phi', 
            'calvo', 
            'delta_0', 'delta_1', 'delta_2', 
            'deltaE_0', 'deltaE_1',
            'rho_theta', 'sigma_nu', 'sigma_eps', 'sigma_i',
            'beta',
            'fcL',
            'kappa',
            'fcH',
            'lerr'
        ]
n_params = len(params)
cols = params + ['SMM']

# Read in data and merge
df = pd.DataFrame(np.genfromtxt('Output/SMM.txt'), columns = cols)
mm = np.genfromtxt('Output/ModelMoments.txt')
cols_mm = cols[:-1] + ['col' + str(i) for i in range(1, np.shape(mm)[1] - len(cols) + 2)]
mm = pd.DataFrame(mm, columns = cols_mm)
df = mm.merge(df, on = cols[:-1], how = 'left')
df.iloc[0,-1] = 0 # reset data SMM

# Filter
df = df.sort_values('SMM').reset_index(drop = True)

###############################################################################
# PLOTS AT BEST FIT FOR ALL MOMENTS
###############################################################################

# Get data and model moments
data = df.loc[0, [x for x in df.columns if 'col' in x]]
model = df.loc[1, [x for x in df.columns if 'col' in x]]

# Initialize plots
fig, ax = plt.subplots(1,2)
ax0 = ax[0]
ax1 = ax[1]

# Income distribution in 2004
tsh_r = 500*np.floor(25347/500)
incomes04 = range(22500, 29000, 500)
moments4 = pd.DataFrame({'Data':data[13:26], 'Model':model[13:26]})
ax0.errorbar(incomes04, moments4['Data'], yerr = np.genfromtxt('Input/SEs/bunching_pre.txt')*1.96, 
             color = 'darkblue', alpha = 0.7, fmt = 'o', capsize = 4, label = 'Data')
ax0.plot(incomes04, moments4['Data'], color = 'darkblue', alpha = 0.7)
ax0.plot(incomes04, moments4['Model'], color = 'maroon', alpha = 0.7, marker = 'o')
ax0.tick_params(axis = 'x', labelrotation = 0)
ax0.axvline(tsh_r, color = 'black', alpha = 0.5)
ax0.set_xticks(range(22500, 29000, 1000))
ax0.set_ylim([6,11])
ax0.set_xticklabels(['{:,}'.format(int(x)) for x in ax0.get_xticks()]) 
ax0.set_yticklabels(['{:,.2%}'.format(x) for x in ax0.get_yticks()/100])
ax0.set_xlabel('HELP Income Relative to Repayment Threshold')
ax0.set_ylabel('Percent of Debtholders within $3,000 of Threshold')
ax0.set_title('Before Policy Change: 2002-2004')
ax0.grid(alpha = 0.3)

# Income distribution in 2005
tsh_r = 35000
incomes05 = range(32500, 39000, 500)
moments5 = pd.DataFrame({'Data':data[26:39], 'Model':model[26:39]})
ax1.errorbar(incomes05, moments5['Data'], yerr = np.genfromtxt('Input/SEs/bunching_post.txt')*1.96, 
             color = 'darkblue', alpha = 0.7, fmt = 'o', capsize = 4, label = 'Data')
ax1.plot(incomes05, moments5['Data'], color = 'darkblue', alpha = 0.7)
ax1.plot(incomes05, moments5['Model'], color = 'maroon', alpha = 0.7, marker = 'o', label = 'Model')
ax1.tick_params(axis = 'x', labelrotation = 0)
ax1.axvline(tsh_r, color = 'black', alpha = 0.5)
ax1.set_xticks(range(32500, 39000, 1000))
ax1.set_ylim([6,11])
ax1.set_xticklabels(['{:,}'.format(int(x)) for x in ax1.get_xticks()]) 
ax1.set_yticklabels(['{:,.2%}'.format(x) for x in ax1.get_yticks()/100])
ax1.set_xlabel('HELP Income Relative to Repayment Threshold')
ax1.set_ylabel('Percent of Debtholders within $3,000 of Threshold')
ax1.set_title('After Policy Change: 2005-2007')
ax1.legend()
ax1.grid(alpha = 0.3)

# Save figure
fig.savefig('Cleaned/fitplots_bunching.pdf', bbox_inches='tight')
