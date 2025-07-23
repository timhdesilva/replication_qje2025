import matplotlib.pyplot as plt
import matplotlib.ticker as mtick
import numpy as np

# Imports
from directories import OUTPATH

# Threshold functions
def th_0304(x):
    if x <= 25347:
        r = 0
    elif x <= 26731:
        r = 0.03
    elif x <= 28805:
        r = 0.035
    elif x <= 33414:
        r = 0.04
    elif x <= 40328:
        r = 0.045
    elif x <= 42447:
        r = 0.05
    elif x <= 45628:
        r = 0.055
    else:
        r = 0.06
    return r
def th_0405(x):
    if x <= 35000:
        r = 0
    elif x <= 38987:
        r = 0.04
    elif x <= 42972:
        r = 0.045
    elif x <= 45232:
        r = 0.05
    elif x <= 48621:
        r = 0.055
    elif x <= 52657:
        r = 0.06
    elif x <= 55429:
        r = 0.065
    elif x <= 60971:
        r = 0.07
    elif x <= 64999:
        r = 0.075
    else:
        r = 0.08
    return r

# Variables of interest
incomes = list(range(20000,70000))
erate = 1
xincomes = np.array([x*erate for x in incomes])
t0304 = [th_0304(x/50929*48571)*100 for x in incomes]
t0405 = [th_0405(x)*100 for x in incomes]
d0304 = np.array([x * th_0304(x/50929*48571) for x in incomes])
d0405 = np.array([x * th_0405(x) for x in incomes])

# Initialize plot
fig, ax = plt.subplots(1,2)
fig.set_size_inches([13,3.5])

# Plot #1: HELP Rates
ax[0].plot(xincomes, t0304, label = '1998-2004', color = 'darkblue', alpha = 0.7)
ax[0].plot(xincomes, t0405, label = '2005-2018', color = 'maroon', alpha = 0.7, linestyle = 'dashed')
ax[0].yaxis.set_major_formatter(mtick.PercentFormatter())
ax[0].set_ylabel('HELP Repayment Rate')
ax[0].set_xlabel('HELP Income in 2005 AUD')
ax[0].set_xticklabels(['{:,}'.format(int(x)) for x in ax[0].get_xticks()])
ax[0].set_ylim(0,9)
ax[0].legend(loc = 'upper left')
usd_aud = 1.18 # 1.18 = exchange rate USD/AUD in 2005 * inflation rate USD to today
axes2 = ax[0].twiny()
axes2.set_xlabel('HELP Income in 2023 USD')
axes2.set_xlim(ax[0].get_xlim())
axes2.set_xticklabels(['{:,}'.format(int(x * usd_aud)) for x in axes2.get_xticks()])

# Plot #2: HELP Dollars
ax[1].plot(xincomes, d0304, color = 'darkblue', alpha = 0.7)
ax[1].plot(xincomes, d0405, color = 'maroon', alpha = 0.7, linestyle = 'dashed')
ax[1].set_ylabel('HELP Repayment in 2005 AUD')
ax[1].set_xlabel('HELP Income in 2005 AUD')
ax[1].set_xticklabels(['{:,}'.format(int(x)) for x in ax[1].get_xticks()])
ax[1].set_yticklabels(['{:,}'.format(int(x)) for x in ax[1].get_yticks()])
axes2 = ax[1].twiny()
axes2.set_xlabel('HELP Income in 2023 USD')
axes2.set_xlim(ax[1].get_xlim())
axes2.set_xticklabels(['{:,}'.format(int(x * usd_aud)) for x in axes2.get_xticks()])
axes3 = ax[1].twinx()
axes3.set_ylabel('HELP Repayment in 2023 USD', labelpad = 8)
axes3.set_ylim(ax[1].get_ylim())
axes3.set_yticklabels(['{:,}'.format(int(x * usd_aud)) for x in axes2.get_yticks()])

# Add grids
for axi in [ax[0], ax[1]]:
    axi.grid(alpha = 0.3)
    
# Save
plt.subplots_adjust(wspace = 0.3)
plt.savefig(OUTPATH / 'help_rates_dollars.pdf', bbox_inches = 'tight')
