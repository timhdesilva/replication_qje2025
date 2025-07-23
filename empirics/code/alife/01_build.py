import os
import gc
import pandas as pd
import numpy as np

# Imports
from directories import taxdir, supdir, datadir, file_debt, file_newflag
from fxns_HELPpayment import HELPpayment, HELPrate
from fxns_globals import low_tsh, fringeb_tsh, indexation_d, indexation_t, min_wage

###############################################################################
# FILE PARAMETERS
###############################################################################

# Important variables
idvar = 'alife_id_001' # unique individual identifier
tvar = 'income_year' # variable for income year

# Range of years available
firstyr = 1991 # first year of tax data
lastyr = 2019 # last year of tax data
firstsyr = 1997 # first year of super data

###############################################################################
# MAKE DATASET OF JUST IMPORTANT HELP INFORMATION
###############################################################################

# Read in debt balances
if not os.path.exists(file_debt):
    raise FileNotFoundError(f"HELP debt balance file not found: {file_debt}")
debtbals = pd.read_csv(file_debt)
debtbals[idvar] = debtbals[idvar].astype(float)
debtbals['help_debt_bal'] = np.maximum(debtbals['help_debt_bal'], 0)

# Read in dataset of flags for new HELP and VSL loans
if not os.path.exists(file_newflag):
    raise FileNotFoundError(f"New HELP/VSL loans file not found: {file_newflag}")
newflag = pd.read_csv(file_newflag)
newflag[idvar] = newflag[idvar].astype(float)
newflag['new_help'] = 1

# Specify variables to keep
keeps_t = ['c_occupation', 'c_gender', 'c_age_30_june', 'c_self_emp_flag', 'c_lodgement_type',
           'c_sa4_id', 'c_sa4_name',
           'i_salary_wage', 'i_allowances', 'i_gov_payments', 'i_gov_pension',
           'i_interest', 'i_div_frank', 'i_div_unfrank', 'i_frank_cr',
           'is_cg_net', 'is_net_rent',
           'ic_total_income_loss', 'ic_taxable_income_loss', 'dc_total_ded',
           'isn_pt_trust_dist_npp_cd', 'ds_pers_super_cont',
           'it_rept_empl_super_cont',
           'c_depend_child', 'sp_status_reported']
keeps_s = ['sb_mem_bal', 'sc_empl_cont', 'sc_pers_cont', 'sc_tot_cont']

# Set data types that need to be set manually
dtype = {
        'c_internet_sale':str, 
        'c_pension_age_eligible':float,
        'on_sato_cd':str,
        'isn_life_insure_bonus_cd':str,
        'on_spouse_housekeeper_cd':str,
        'dsn_pt_other_ded_npp_cd':str,
        'dsn_pt_other_ded_pp_cd':str,
        'on_pto_vet_cd':str,
        'isn_fsi_empl_pension_cd':str,
        'isn_had_transfer_nr_trust':str,
        'osn_other_non_ref_cd':str,
        'in_etp_cd':str,
        'in_super_lsum_cd':str,
        'isn_pt_trust_dist_pp_cd':str,
        'on_sato_vet_cd':str,
        'ml_full_exempt_cd':str,
        'sp_birthday_pre1971':str,
        'on_sapto_vet_cd':str,
        'osn_other_ref_cd':str,
        'sp_birthday_pre1952':str,
}

# Loop through years
for yr in range(firstyr, lastyr + 1):

    # Read in aLife tax dataset onto disk
    if not os.path.exists(taxdir):
        raise FileNotFoundError(f"Tax directory not found: {taxdir}")
    os.chdir(taxdir)
    df = pd.read_csv(f'aLife10_tax_{yr}.csv', dtype = dtype)
    df[idvar] = df[idvar].astype(float)

    # Compute HELP income
    df['help_income'] = np.maximum(df['ic_taxable_income_loss'], 0)
    adds = ['help_income']
    if yr >= 2000:
        adds += ['it_rept_fringe_benefit']
    if yr >= 2006:
        adds += ['isn_fsi_exempt_empl']
    if yr >= 2010:
        adds += ['it_property_loss', 'it_invest_loss', 'it_rept_empl_super_cont']
    df[adds] = df[adds].fillna(0)
    if yr >= 2000:
        df['it_rept_fringe_benefit'] *= ((df['it_rept_fringe_benefit'] >= fringeb_tsh[yr]).astype(int))
    df['help_income'] = df[adds].sum(axis = 1)

    # Create different labor income components measures
    if 'i_attributed_psi' in df.columns:
        df['psi_b9'] = df['i_attributed_psi'].fillna(0)
    else:
        df['psi_b9'] = np.nan
    if 'is_psi_net' in df.columns:
        df['psi_b14'] = df['is_psi_net'].fillna(0)
    else:
        df['psi_b14'] = np.nan
    if 'pt_is_pship_dist_pp' in df.columns and 'pt_is_pship_dist_npp' in df.columns:
        df['pship_b13'] = df[['pt_is_pship_dist_pp', 'pt_is_pship_dist_npp']].fillna(0).sum(axis = 1)
    else:
        df['pship_b13'] = np.nan
    df['solet_b15'] = df[['is_bus_pp', 'is_bus_npp']].fillna(0).sum(axis = 1)

    # Create trust income measure
    if 'pt_is_trust_dist_npp' in df.columns and 'pt_is_frank_dist_trust_npp' in df.columns:
        df['trustNPP_b13'] = df[['pt_is_trust_dist_npp', 'pt_is_frank_dist_trust_npp']].fillna(0).sum(axis = 1)
    elif 'pt_is_trust_dist_npp' in df.columns:
        df['trustNPP_b13'] = df['pt_is_trust_dist_npp'].fillna(0)
    else:
        df['trustNPP_b13'] = np.nan

    # Keep only people who are not exempt from repayment
    df = df[df['c_resident'] == 'Y']
    if yr >= 1997:
        df = df[df['ml_full_exempt_days'] + df['ml_half_exempt_days'] == 0]

    # Create capital income measure
    capitalvars = ['i_annuities_txd', 'i_annuities_untxd',
                   'i_annuities_lsum_txd', 'i_annuities_lsum_untxd',
                   'i_super_lsum_txd', 'i_super_lsum_untxd',
                   'i_interest', 'i_div_frank', 'i_div_unfrank',
                   'trustNPP_b13', 'is_cg_net', 'is_net_rent']
    df['capital_income'] = 0
    for v in capitalvars:
        if v in df.columns:
            df['capital_income'] += df[v].fillna(0)

    # Keep only tax variables of interest and bring into memory
    keeps = [x for x in keeps_t if x in df.columns] + [x for x in df.columns if 'hc_' in x]
    keeps += ['help_income', 'capital_income', 'psi_b9', 'psi_b14',
              'pship_b13', 'solet_b15', 'trustNPP_b13']
    df = df[[idvar, tvar] + keeps]

    # Merge in aLife super dataset with variables of interest
    if yr >= firstsyr:
        os.chdir(supdir)
        df_s = pd.read_csv(f'aLife10_super_{yr}.csv')
        df_s[idvar] = df_s[idvar].astype(float)
        df_s = df_s[[idvar] + keeps_s]
        df = df.merge(df_s, on = idvar, how = 'left')
        del df_s
        gc.collect()

    # Merge in debt balances dataset in year t
    df = df.merge(debtbals, on = [idvar, tvar], how = 'left')

    # Merge in debt balances dataset in year t + 1
    if yr < lastyr:
        debt_t1 = debtbals[debtbals[tvar] == yr + 1].copy().drop(tvar, axis = 1)
        debt_t1 = debt_t1.rename(columns = {'help_debt_bal':'help_debt_bal_t1'})
        df = df.merge(debt_t1, on = idvar, how = 'left')
        del debt_t1
        gc.collect()
    else:
        df['help_debt_bal_t1'] = 0
        
    # Merge in debt balances dataset in year t - 1
    if yr > firstyr:
        debt_l1 = debtbals[debtbals[tvar] == yr - 1].copy().drop(tvar, axis = 1)
        debt_l1 = debt_l1.rename(columns = {'help_debt_bal':'help_debt_bal_l1'})
        df = df.merge(debt_l1, on = idvar, how = 'left')
        del debt_l1
        gc.collect()
    else:
        df['help_debt_bal_l1'] = 0
        
    # Fill missing values from merges
    misscols = [x for x in ['help_debt_bal', 'help_debt_bal_t1', 'help_debt_bal_l1'] + keeps_s if x in df.columns]
    df[misscols] = df[misscols].fillna(0)
        
    # Merge in flag for whether individual had new loans in year t
    df = df.merge(newflag, on = [idvar, tvar], how = 'left')
    df['new_help'] = df['new_help'].fillna(0)

    # Calculate HELP rates and repayments in current year
    df['help_rate'] = df['help_income'].apply(HELPrate, args = (yr,))
    df['help_reqpay'] = df['help_income'].apply(HELPpayment, args = (yr,))

    # Calculate the change in HELP rates you'd have experienced in 2004-2005
    # if income was deflated back
    level_04 = np.product([indexation_t[x] for x in range(1991,2005)])
    level_05 = np.product([indexation_t[x] for x in range(1991,2006)])
    level_yr = np.product([indexation_t[x] for x in range(1991,yr + 1)])
    df['help_rate_05'] = (df['help_income']*level_05/level_yr).apply(HELPrate, args = (2005,))
    df['help_rate_04'] = (df['help_income']*level_04/level_yr).apply(HELPrate, args = (2004,))

    # Calculate entrepreneurship and labor income
    df['entrep'] = (np.abs(df[['psi_b9', 'pship_b13', 'solet_b15']]).max(axis = 1) > 0).astype(int)
    laborvars = ['i_salary_wage', 'i_allowances', 'psi_b9', 'psi_b14', 'pship_b13', 'solet_b15']
    df['labor_income'] = df[laborvars].fillna(0).sum(axis = 1)
    
    # Calculate voluntary super contributions
    if 'it_rept_empl_super_cont' in df.columns and 'sc_pers_cont' in df.columns:
        df['voluntary_super'] = df[['sc_pers_cont', 'it_rept_empl_super_cont']].fillna(0).sum(axis = 1)
        df['make_voluntary_super'] = (df['voluntary_super'] != 0).astype(int)
    else:
        df['voluntary_super'] = np.nan
        df['make_voluntary_super'] = np.nan

    # Final variable formatting
    df['c_gender'] = df['c_gender'].astype(str)
    df[tvar] = df[tvar].astype(int)

    # Calculate other variables of interest
    df['help_has_debt'] = (df['help_debt_bal'] > 0).astype(int)
    df['threshold'] = low_tsh[yr]
    df['indexation_d'] = indexation_d[yr]
    df['indexation_t'] = indexation_t[yr]
    df['tindex_05'] = level_05/level_yr
    df['dindex_05'] = np.product([indexation_d[x] for x in range(1991,2006)])/\
        np.product([indexation_d[x] for x in range(1991,yr + 1)])
    if 'hc_max_repayment' in df.columns:
        df['help_benefit'] = df['hc_max_repayment'] - df['hc_repayment']
    else:
        df['help_benefit'] = np.nan
    if 'c_occupation' in df.columns:
        df['c_occupation1'] = df['c_occupation'].fillna(0).astype(str).apply(lambda x: x[0]).astype(int)
    df['net_deduc'] = -(df['help_income'] - df[['labor_income', 'capital_income']].sum(axis = 1))
    df['interest_dividend'] = df[['i_interest', 'i_div_frank', 'i_div_unfrank']].sum(axis = 1)
    df['net_deduc_id'] = -(df['help_income'] - df[['labor_income', 'interest_dividend']].sum(axis = 1))
    if 'isn_pt_trust_dist_npp_cd' in df.columns:
        df['has_trust'] = (df['isn_pt_trust_dist_npp_cd'].isin(['S', 'T', 'I'])).astype(int)
    else:
        df['has_trust'] = 0
    df['female'] = (df['c_gender'] == 'F').astype(int)
    df['below_min'] = (df['labor_income'] <= min_wage[yr]*38*52).astype(int)
    df['below_tsh'] = (df['help_income'] <= low_tsh[yr]).astype(int)

    # Output to stata
    os.chdir(datadir)
    df.to_stata(f'help_{yr}.dta', write_index = False)
    del df
    gc.collect()
    print(f'Finished year: {yr}')
