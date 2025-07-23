import os
import pandas as pd
import numpy as np
import pickle

maindir = os.getcwd()
idir = maindir + '/Input'
odir = maindir + '/Output'
adir = maindir + '/Cleaned'

# List of additional columns in OptimalPolicy.txt files outside of parameters
op_cols = ['-V0', 'budget', 'penterm', 'objective', 'iter_pen']

#######################################################################################
# FORMATTING OF OPTIMAL POLICY RESULTS
#######################################################################################

# Collect list of OptimalPolicy files
files = [x for x in os.listdir(odir) if 'OptimalPolicy' in x]

# Identify unique policies and names
policies = np.unique([int(x.split('OptimalPolicy')[1].split('_')[0]) for x in files])

# Read in consumption equivalent gains and cash transfers
cg0 = pd.DataFrame(np.genfromtxt(f'{odir}/ConsumptionEquivalent_0.txt'), columns = ['g', 'V'])
cg1 = pd.DataFrame(np.genfromtxt(f'{odir}/ConsumptionEquivalent_1.txt'), columns = ['g', 'V'])
ev0 = pd.DataFrame(np.genfromtxt(f'{odir}/CashTransfers_0.txt'), columns = ['w', 'V'])
ev1 = pd.DataFrame(np.genfromtxt(f'{odir}/CashTransfers_1.txt'), columns = ['w', 'V'])

# Loop through policies
dfs = {}
dfs_r = {}
for p in policies:
    # Make two dataframes for each policy
    df0 = np.genfromtxt(f'{odir}/OptimalPolicy{p}_0.txt', ndmin = 2)
    df1 = np.genfromtxt(f'{odir}/OptimalPolicy{p}_1.txt', ndmin = 2)
    n_theta = np.shape(df0)[1] - len(op_cols)
    thetas = [f'theta_{i}' for i in range(1, n_theta+1)]
    df0 = pd.DataFrame(df0, columns = thetas + op_cols)
    df1 = pd.DataFrame(df1, columns = df0.columns)
    # Compute welfare gains
    df0['gain_c'] = np.interp(-1*df0['-V0'], cg0['V'], cg0['g'])
    df0['gain_cl'] = -1*df0['-V0']/cg0.query('g == 0')['V'].iloc[0] - 1
    df1['gain_c'] = np.interp(-1*df1['-V0'], cg1['V'], cg1['g'])
    df1['gain_cl'] = -1*df1['-V0']/cg1.query('g == 0')['V'].iloc[0] - 1
    df0['EV'] = np.interp(-1*df0['-V0'], ev0['V'], ev0['w'])
    df1['EV'] = np.interp(-1*df1['-V0'], ev1['V'], ev1['w'])
    df0['counterfactual'] = 1
    df1['counterfactual'] = 0
    if p > 1:
        cf_eval = df0.iloc[[-1]]
        df0 = df0.iloc[:-1]
        cf_eval['penterm'] = np.nan
        cf_eval['counterfactual'] = -1
        cf_eval['gain_c'] = np.interp(-1*cf_eval['-V0'], cg1['V'], cg1['g'])
        cf_eval['gain_cl'] = -1*cf_eval['-V0']/cg1.query('g == 0')['V'].iloc[0] - 1
        cf_eval['EV'] = np.interp(-1*cf_eval['-V0'], ev1['V'], ev1['w'])
    # Take last value from each dataframe, which is what TikTak said was best, and concatenate, adding to df list
    if p == 1:
        dfs[p] = pd.concat([df0.iloc[[-1]], df1.iloc[[-1]]], ignore_index = True)
    else:
        dfs[p] = pd.concat([df0.iloc[[-1]], df1.iloc[[-1]], cf_eval], ignore_index = True)

# Output to pickle
with open(f'{adir}/OptimalPolicies.pcl', 'wb') as f:
    pickle.dump(dfs, f)



#######################################################################################
# FORMATTING OF VALUE FUNCTIONS OUTPUT FOR HETEROGENEITY
#######################################################################################

# Collect list of files
filesV = [x for x in os.listdir(odir) if 'avgV0_aD' in x]

# List of three dimensions
policiesV = np.unique([int(x.replace('avgV0_aD', '')[:-4]) for x in filesV if 'base' not in x]) # policy ids
h_dim = ['', 'A', 'i', 'D'] # heterogeneity dimension

# Loop over to read in files
results = {}
for p in ['base'] + list(policiesV):
    for h in h_dim:
        txt = np.genfromtxt(f'{odir}/avgV0_a{h}{p}.txt')
        if p not in results:
            results[p] = {}
        results[p][h] = txt

# Output to pickle
with open(f'{adir}/OptimalPolicies_Vs.pcl', 'wb') as f:
    pickle.dump(results, f)



#######################################################################################
# FORMATTING OF INITIAL VALUE FUNCTIONS
#######################################################################################

# Collect list of files
filesV0 = [x for x in os.listdir(odir) if 'initialV' in x]

# Only output results if base is there
if len([x for x in filesV0 if 'base' in x]) > 0:

    # Read in base file and format
    base = np.genfromtxt(f'{odir}/initialVbase.txt', ndmin = 2)
    base = pd.DataFrame(base, columns = ['V0_base', 'A0', 'nu0', 'D0'])
    base = base[['A0', 'nu0', 'D0', 'V0_base']]

    # Loop over other files merging in
    policiesV0 = [int(x.replace('initialV', '')[:-4]) for x in filesV0 if 'base' not in x]
    for p in policiesV0:
        V0_p = np.genfromtxt(f'{odir}/initialV{p}.txt')
        base[f'V0_{p}'] = V0_p

    # Output to pickle
    with open(f'{adir}/OptimalPolicies_initialV.pcl', 'wb') as f:
        pickle.dump(base, f)
