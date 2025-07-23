import os
import pandas as pd
import numpy as np
import pickle

maindir = os.getcwd()
idir = maindir + '/Input'
odir = maindir + '/Output'
adir = maindir + '/Cleaned'

# Mapping from policy numbers to policy names
policies = {
    0:'Debt Cancellation',
    -2:'10-Year Fixed Repayment',
    -3:'25-Year Fixed Repayment',
    -4:'HELP 2004',
    -5:'HELP 2005',
    -6:'10-Year/10-Year\nFixed',
    -101:'US IBR',
    -102:'US SAVE',
    -103:'US IBR\n+ Fixed Cap',
    -104:'US SAVE\n+ Fixed Cap',
    -201:'US IBR\n+ Forgiveness',
    -202:'US SAVE\n+ Forgiveness',
    -402:'Purdue ISA',
    -403:'Purdue ISA\n+ Threshold',
    -1010:'US IBR\n+ Equity',
}

# Columns of ComparePolicies_.txt
cols = [
    'policy_id', 'R_d', 'DebtPayoff', 
    'meanV', 'meanD0', 'meanrepay', 'repay_pdv', 'meanTT', 'TT_pdv', 
    'agerepay', 'frac_norepay',
    'repay_pdv_cfL', 'TT_pdv_cfL',
    ]

# Read in data and merge policies, setting as index
df = pd.DataFrame(np.genfromtxt(f'{odir}/ComparePolicies.txt'), columns = cols)
df['policy'] = df['policy_id'].apply(lambda x: policies.get(x))
df = df.set_index('policy')

# Add equivalent variation and consumption-equivalent gains
ev = pd.DataFrame(np.genfromtxt(f'{odir}/CashTransfers_CP.txt'), columns = ['w', 'V'])
cg = pd.DataFrame(np.genfromtxt(f'{odir}/ConsumptionEquivalent_CP.txt'), columns = ['g', 'V'])
df['EV'] = np.interp(df['meanV'], ev['V'], ev['w'])
df['gain_c'] = np.interp(df['meanV'], cg['V'], cg['g'])

# Calculate change in PDV of repayments relative to first policy
df['change_repay'] = (df['repay_pdv'] - df['repay_pdv'].iloc[0])
df['change_tt'] = (df['TT_pdv'] - df['TT_pdv'].iloc[0])

# Calculate change in GBC relative to first policy, with and without endogenous labor supply
df['total_change'] = df['change_repay'] + df['change_tt']
df['change_cfL'] = (df['repay_pdv_cfL'] - df['repay_pdv'].iloc[0]) + (df['TT_pdv_cfL'] - df['TT_pdv'].iloc[0])
df['change'] = df['total_change'] - df['change_cfL']
        
# Calculate MVPF
df['MVPF'] = -df['EV'] / df['total_change']
    
# Output
df.to_pickle(f'{adir}/ComparePolicies.pcl')
