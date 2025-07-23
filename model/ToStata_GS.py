import os
import pandas as pd
import numpy as np

maindir = os.getcwd()
idir = maindir + '/Input'
odir = maindir + '/Output'
adir = maindir + '/Cleaned'

############################################################################################################
# LIFECYCLE/PANEL RESULTS
############################################################################################################

cols_lc = ['iter', 'p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'age', 'V', 'Y', 'C', 'L', 'A', 'D', 'repay', 'TT', 'flag']
cols_pn = ['id', 'E', 'switchIJ', 'age', 'policy', 'since', 'V', 'rate', 'Y', 'W', 'theta', 'epsilon', 
           'C', 'L', 'A', 'D', 'repay', 'TT', 'flag', 'soltype']

files = [x for x in os.listdir(odir) if 'Lifecycle' in x or 'Panel' in x]

for f in files:
    os.chdir(odir)
    txt = np.genfromtxt(f)
    if 'Lifecycle' in f:
        cols = cols_lc
    elif 'Panel' in f:
        cols = cols_pn
    df = pd.DataFrame(txt, columns = cols)
    if 'Panel1' in f:
        # df = df.query('V != -99')
        df['year'] = 2005 + df['since']
    os.chdir(adir)
    df.to_stata(f.replace('.txt', '.dta'), write_index = False)
    

  
############################################################################################################
# SMM RESULTS
############################################################################################################  
    
# Parameters and SMM
cols = ['gamma', 'invEIS', 
        'phi', 'cprob', 
        'delta_0', 'delta_1', 'delta_2',
        'deltaE_0', 'deltaE_1', 
        'rho_theta', 'sigma_nu', 'sigma_eps', 'sigma_i', 
        'beta', 'fcL', 'kappa', 'fcH', 'lerr']
os.chdir(odir)
txt = np.genfromtxt('SMM_GS.txt')
if txt.ndim == 1:
    txt = txt.reshape(1, -1)
df = pd.DataFrame(txt, columns = cols + ['SMM'])
os.chdir(adir)
df.to_stata('SMM_GS.dta', write_index = False)


# Moments
moms = []
# moms = ['Mean_Income']
moms += [f'SD{i}' for i in [22,32,42,52,62]]
moms += ['Age', 'Age2']
moms += ['P10_1yr', 'P10_5yr']
moms += ['P90_1yr', 'P90_5yr']
moms += ['EP_Level', 'EP_Slope']
#moms += [f'_2004_{i}' for i in range(20000, 30000, 500)]
#moms += [f'_2005_{i}' for i in range(30000, 40000, 500)]
moms += [f'Pre_{i}' for i in range(22500, 29000, 500)]
moms += [f'Post_{i}' for i in range(32500, 39000, 500)]
# moms += [f'CapitalIncome_{i}' for i in range(20, 64, 5)]
# moms += ['CapitalIncome_40']
# moms += ['Ratio25', 'Ratio35', 'Ratio38']
# moms += ['Ratio35_Q1', 'Ratio35_Q4']
moms += ['Ratio35_DebtHet']
moms += ['Mean_Hours', 'Frac_NoAdjust', 'Kurt_dlogL']
moms += ['Bunch05_Bunch04']
# moms += ['Bunch05_Bunch05', 'Old05_Bunch04', 'Bunch04_Bunch05', 'New04_Bunch05', 'Persist0518']
# moms += ['DD_D_01', 'DD_D_06', 'DD_D_01_young', 'DD_D_01_old']
cols_mom = cols + moms
os.chdir(odir)
txt = np.genfromtxt('ModelMoments_GS.txt')
if txt.ndim == 1:
    txt = txt.reshape(1, -1)
df = pd.DataFrame(txt, columns = cols_mom)
os.chdir(adir)
df.to_stata('ModelMoments_GS.dta', write_index = False)
    

############################################################################################################
# INCOME DISTRIBUTION
############################################################################################################  

# cols = ['income'] + ['percent_' + str(yr) for yr in range(2004, 2009)]
# os.chdir(idir)
# dist_d = pd.DataFrame(np.genfromtxt('incdistribution_debt.txt'), columns = cols)
# dist_d['iter'] = 0
# dist_nd = pd.DataFrame(np.genfromtxt('incdistribution_nodebt.txt'), columns = cols)
# dist_nd['iter'] = 0

# files = [x for x in os.listdir(odir) if 'incdistribution' in x]
# os.chdir(odir)
# for f in files:
#     dist = pd.DataFrame(np.genfromtxt(f), columns = cols)
#     iter = f[f.find('debt') + len('debt'):f.find('.txt')]
#     dist['iter'] = int(iter)
#     if 'nodebt' in f:
#         dist_nd = pd.concat([dist_nd, dist], ignore_index = True)
#     else:
#         dist_d = pd.concat([dist_d, dist], ignore_index = True)
# os.chdir(adir)
# dist_d.to_stata('incdistribution_debt.dta', write_index = False)
# dist_nd.to_stata('incdistribution_nodebt.dta', write_index = False)
