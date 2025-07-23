import pandas as pd
import numpy as np
from numpy import ndarray
import pickle
import matplotlib.pyplot as plt
from tabulate import tabulate
import matplotlib.pyplot as plt

# Cycler for plots
from cycler import cycler
cyc = (
        cycler(color=['darkblue', 'maroon']) *
        cycler(linestyle=['solid', 'dashed', 'dashdot'])
      ) + cycler(linewidth=[1, 1, 1, 3, 3, 3]
)
plt.rc('axes', prop_cycle = cyc)

# Dictionary of contract labels and for baseline policy
names = {
    1:'Debt Cancellation (Upper Bound)',
    2:'Income-Contingent\nLoan',
    3:'Income-Contingent\nLoan + 20 Year\nForgiveness',
    4:'Income Sharing\nAgreement\n(9 Years)',
    5:'Income Sharing\nAgreement\n(9 Years) + Kink',
    6:'Income Sharing\nAgreement\n($a_R-a_0$ Years)',
    7:'Income Sharing\nAgreement\n($a_R-a_0$) + Kink',
    8:'Income-Contingent\nLoan with Notch',
    9:'25-Year Fixed\nRepayment +\nForbearance',
    10:'Constant Payment\n+ Unemployment\nForbearance',
}
names_flat = {k: v.replace('\n', ' ') for k, v in names.items()}
base_lab = '25-Year Fixed Repayment'

# Parameters for plots
plotD0 = 17448 # average initial debt in the model
maxYplot = 70000

# Read in data
with open('Cleaned/OptimalPolicies.pcl', 'rb') as f:
    dfs = pickle.load(f)
with open('Cleaned/OptimalPolicies_initialV.pcl', 'rb') as f:
    V0s = pickle.load(f)
    
# Delete any contracts from df that violated budget constraint
dfs0 = dfs.copy()
for key, val in dfs0.items():
    if key > 1 and val['penterm'].max() > 0:
        del dfs[key]
        print(f"Deleted contract {key} = '{names_flat[key]}' due to budget constraint violation!")
del dfs0

# Function for US Fixed Repayment at ij_t=1
def US_Fixed(theta:ndarray, D0:float, Tp:int):
    if abs(theta[0] - 1) < 1e-5:
        irate = 1e-5
    else:
        irate = theta[0] - 1
    return irate/(1 - (1 + irate)**(-Tp))*D0

# Function for calculating repayment of a given contract
def repayment(incomes_raw:np.ndarray, policy:int):
    incomes = incomes_raw / 40000
    if policy in [2, 3, 5, 7]:
        theta = dfs[policy].query('counterfactual == 0')[['theta_1', 'theta_2']].iloc[0].values
        repay = theta[0] * np.maximum(incomes - theta[1], 0)
    elif policy in [4, 6]:
        theta = dfs[policy].query('counterfactual == 0')[['theta_1']].iloc[0].values
        repay = theta[0] * incomes
    elif policy == 8:
        theta = dfs[policy].query('counterfactual == 0')[['theta_1', 'theta_2']].iloc[0].values
        repay = theta[0]*incomes
        repay = repay * (incomes >= theta[1]).astype(int)
    elif policy == 9:
        theta = dfs[policy].query('counterfactual == 0')[['theta_1']].iloc[0].values
        repay = US_Fixed(theta, plotD0/40000, 25)
        repay = repay * (incomes >= 16863 / 40000).astype(int)
    elif policy == 10:
        theta = dfs[policy].query('counterfactual == 0')[['theta_1']].iloc[0].values
        repay = theta[0] * (incomes >= 16863 / 40000).astype(int)
    else:
        print(f'Policy {policy} repayment not implemented!')
        repay = np.zeros_like(incomes)
    # Rescale
    repay *= 40000
    # Cap by initial debt balances
    if policy in [2, 3, 8, 10]:
        repay = np.minimum(repay, plotD0)
    return repay

# Function to plot optimal contracts
def plot_contracts(policies_keep:list):
    # Range of incomes
    incomes = np.arange(1, maxYplot)
    # Calculate repayments
    repays = [repayment(incomes, p) for p in policies_keep]
    max_y = np.max([np.max(r) for r in repays])
    # Make plot
    fig, ax = plt.subplots(1,1)
    fig.set_size_inches(8,5)
    for i, p in enumerate(policies_keep):
        repay = repays[i]
        ax.plot(incomes, repay, label = names_flat[p])
    # Format plot and save
    ax.legend()
    ax.set_xlim((incomes[0], incomes[-1]))
    ax.set_ylim((-100, max_y))
    ax.grid(alpha = 0.3)
    ax.xaxis.set_major_formatter('${x:,.0f}')
    ax.yaxis.set_major_formatter('${x:,.0f}')
    ax.set_xlabel('Labor Income: $y_{ia}$')
    ax.set_ylabel('Repayment with Average Initial Debt: $d_{ia}$')
    plt.savefig('Cleaned/optimalcontracts.pdf', bbox_inches = 'tight')

# Function to get contract parameters of interest based on policy
def optimal_parameters(policy:int, cf:int):
    if policy in [2, 3, 5, 7, 8]:
        psi, K = dfs[policy].query(f'counterfactual == {cf}')[['theta_1', 'theta_2']].iloc[0].values
    elif policy in [4, 6]:
        psi = dfs[policy].query(f'counterfactual == {cf}')[['theta_1']].iloc[0].values[0]
        K = np.nan
    elif policy in [9, 10]:
        psi = dfs[policy].query(f'counterfactual == {cf}')[['theta_1']].iloc[0].values[0] - 1
        K = np.nan
    else:
        psi, K = np.nan, np.nan
    K *= 40000.
    return np.array([psi, K])

# Function for formatting numerical values as string percents
def fmt_pct(x:float, digits:int=2):
    if np.isnan(x):
        return 'nan'
    else:
        return f"{x*100.:.{digits}f}%"

# Function for formatting numerical values as string dollars with no decimals
def fmt_dollars(x:float):
    if np.isnan(x):
        return 'nan'
    else:
        out = f"-${abs(int(x)):,.2f}" if x < 0 else f"${int(x):,.2f}"
        out = out.replace('.00', '')
        return out
        
# Function to form results needed for tabulation for a given policy
def tabulate_policy(policy:int):
    # Get welfare gains
    gain_c = dfs[policy].query('counterfactual == 0')['gain_c'].iloc[0]
    ev = dfs[policy].query('counterfactual == 0')['EV'].iloc[0]
    # Get contract parameters
    psi, K = optimal_parameters(policy, 0)
    psi_cf, K_cf = optimal_parameters(policy, 1)
    # Format values
    gain_c = fmt_pct(gain_c)
    ev = fmt_dollars(ev)
    if psi > 0.1:
        psi = fmt_pct(psi, 0)
    elif psi > 0.01:
        psi = fmt_pct(psi, 1)
    else:
        psi = fmt_pct(psi)
    if psi_cf > 0.1:
        psi_cf = fmt_pct(psi_cf, 0)
    elif psi_cf > 0.01:
        psi_cf = fmt_pct(psi_cf, 1)
    else:
        psi_cf = fmt_pct(psi_cf)
    K = fmt_dollars(K)
    K_cf = fmt_dollars(K_cf)
    # Format output, which corresponds to a rows of dataframe
    row = {
        'col1':names_flat[policy],
        'col2':psi,
        'col3':K,
        'col6':ev,
        'col7':gain_c,
        'col4':psi_cf,
        'col5':K_cf,
    }
    return row

# Function to make .tex table for results from multiple policies
def make_tex(policies_keep:list, name:str):
    # Collect data in dataframe
    rows = [tabulate_policy(p) for p in policies_keep]
    df = pd.DataFrame(rows).set_index('col1', drop = True)
    # Make value replacements
    df = df.replace({'nan':'.'})
    # Make table and do final cleaning
    table = tabulate(df, tablefmt = 'latex_booktabs', stralign = 'center', numalign = 'center', headers = 'keys', disable_numparse = True)
    tex_dict = {
        'col1':r'Contract Space: $p$',
        'col2':r'$\psi_p$',
        'col3':r'$K_p$',
        'col4':r'$\psi_p^{\ell \text{ fixed}}$',
        'col5':r'$K_p^{\ell \text{ fixed}}$',
        'col6':r'$\pi_p$',
        'col7':r'$g_p$',
        '\$a\_R-a\_0\$':r'$a_R-a_0$',
    }
    table = table.replace('begin{tabular}{c', 'begin{tabular}{l')
    for c in tex_dict.keys():
        table = table.replace(c, tex_dict[c])
    print(table)
    # Write to .tex file
    with open(f'Cleaned/table{name}.tex', 'w') as f:
        f.write(table)

# Produce results
plot_contracts([2, 8, 3, 9, 4, 6])
make_tex([2, 8, 3, 9, 4, 6], '')
