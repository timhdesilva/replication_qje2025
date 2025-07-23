import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from tabulate import tabulate

from fxns_HELPpayment import HELPpayment

# Cycler for plots
from cycler import cycler
cyc = (
        cycler(color=['darkblue', 'maroon']) *
        cycler(linestyle=['solid', 'dashed', 'dashdot'])
      ) + cycler(linewidth=[1, 1, 1, 3, 3, 3]
)
plt.rc('axes', prop_cycle = cyc)

# Parameters for plots
plotD0 = 17448 # average initial debt in model in dollars
maxYplot = 70000

# Read in output
df0 = pd.read_pickle('Cleaned/ComparePolicies.pcl')

# Get label for base policy, which is first one
base_lab = df0.index[0]

# Replace infinity MVPFs with 99
df0.loc[(df0['EV'] > 0) & (df0['total_change'] > 0), 'MVPF'] = 99

# Function for US Fixed Repayment
def US_Fixed(R_d:float, D0:float, Tp:int):
    if abs(R_d - 1) < 1e-5:
        irate = 1e-5
    else:
        irate = R_d - 1
    return irate/(1 - (1 + irate)**(-Tp))*D0

# Function for US IBR
def US_IBR(Y:float, R_d:float, D0:float, rate:float, pov_frac:float, cap_fixed:bool):
    pov = 14580*0.65*1.3
    repay = rate * np.maximum(Y - pov_frac * pov, 0.)
    if cap_fixed:
        repay = np.minimum(repay, US_Fixed(R_d, D0, 25))
    repay = np.minimum(repay, R_d * D0)
    return repay

# Function for calculating repayments
def repayment(incomes:np.ndarray, policy:int, D0:float):
    # Calculate repayment
    if policy == 0:
        repay = np.zeros_like(incomes)
    elif policy == -2:
        repay = np.ones_like(incomes) * US_Fixed(1.0, D0, 10)
    elif policy == -3:
        repay = np.ones_like(incomes) * US_Fixed(1.0, D0, 25)
    elif policy == -4:
        repay = np.minimum(np.vectorize(HELPpayment, otypes = [int])(incomes * 48571/50929, 2004), D0)
    elif policy == -5:
        repay = np.minimum(np.vectorize(HELPpayment, otypes = [int])(incomes, 2005), D0)
    elif policy == -101 or policy == -201:
        repay = US_IBR(incomes, 1.0, D0, 0.1, 1.5, False)
    elif policy == -102 or policy == -202:
        repay = US_IBR(incomes, 1.0, D0, 0.05, 2.25, False)
    elif policy == -103:
        repay = US_IBR(incomes, 1.0, D0, 0.1, 1.5, True)
    elif policy == -104:
        repay = US_IBR(incomes, 1.0, D0, 0.05, 2.25, True)
    elif policy == -402:
        repay = 0.04 * incomes
    return repay

# Create data for plot of contracts
incomes = np.arange(1, maxYplot)*1.
policies_plot = [-3, -4, -5, -101, -102, -402]
repays = [repayment(incomes, p, plotD0) for p in policies_plot]
max_y = np.max([np.max(r) for r in repays])

# Make plot if contracts
fig, ax = plt.subplots()
fig.set_size_inches(8,5)
for i, p in enumerate(policies_plot):
    repay = repays[i]
    label_i = df0.query("policy_id == @p").index[0]
    if p == -3:
        label_i = 'Benchmark Fixed Repayment'
    line, = ax.plot(incomes, repay, label = label_i)
ax.set_xlim((incomes[0], incomes[-1]))
ax.set_ylim((-100, max_y))
ax.legend()
ax.grid(alpha = 0.3)
ax.xaxis.set_major_formatter('${x:,.0f}')
ax.yaxis.set_major_formatter('${x:,.0f}')
ax.set_xlabel('Labor Income: $y_{ia}$')
ax.set_ylabel('Repayment with Average Initial Debt: $d_{ia}$')
plt.savefig('Cleaned/existingcontracts.pdf', bbox_inches = 'tight')

# Function to make table of results
def make_tex(policies_keep:list):
    # Set key function parameters
    tab_cols = ['EV', 'change_repay', 'change_tt', 'total_change', 'MVPF', 'gain_c']
    labels = ['$\pi_p$',
              'Change in Repayments',
              'Change in Taxes & Transfers',
              'Total Fiscal Impact',
              'MVPF',
              '$g_p$']
    dollars = [True, True, True, True, False, False] # whether tab_cols are in dollars
    percents = [False, False, False, False, False, True] # whether tab_cols are in percent
    # Keep policies of interest
    if policies_keep != []:
        df = df0[df0.policy_id.isin(policies_keep)].copy()
    # Filter to columns of interest
    df = df[tab_cols].copy()
    # Convert consumption-equivalent gains to %
    df['gain_c'] = 100 * df['gain_c']
    # Round to desired level and produce strings
    for i, c in enumerate(tab_cols):
        if dollars[i]:
            df[c] = df[c].astype(int).apply(lambda x: f"-${abs(x):,}" if x < 0 else f"${x:,}")
        else:
            df[c] = df[c].apply(lambda x: f"{x:.2f}")
        if percents[i]:
            df[c] = df[c] + '%'
    # Make value replaces
    replaces = {'nan': '.', '0.0': '0', '99.00':'infinity'}
    df = df.replace(replaces)
    # Clean indices to be on one line
    df.index.name = 'Policy: $p$'
    df.index = df.index.str.replace('\n', ' ')
    # Rename columns
    df = df.rename(columns = {old: new for old, new in zip(tab_cols, labels)})
    # Make table and do final cleaning
    table = tabulate(df, tablefmt = 'latex_booktabs', stralign = 'center', numalign = 'center', headers = 'keys', disable_numparse = True)
    table = table.replace('begin{tabular}{c', 'begin{tabular}{l')
    table = table.replace(r'\$p\$', '$p$')
    table = table.replace(r'\$\textbackslash{}pi\_p\$', r'$\pi_p$')
    table = table.replace(r'\$g\_p\$', '$g_p$')
    table = table.replace('infinity', '$\infty$')
    table = table.replace('Change in', r'$\Delta$')
    table = table.replace('Total Fiscal Impact', r'$\Delta \mathcal{G}_p$')
    # Write to .tex file
    f = open('Cleaned/table.tex', 'w')
    _ = f.write(table)
    f.close()

# Make table of relevant policies
make_tex([-4, -5, -101, -102, -103, -104, -201, -202, -402, 0])
