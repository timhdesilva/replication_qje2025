import os
import numpy as np
import pandas as pd
import dask.dataframe as dd
import dask.array as da
from dask.diagnostics import ProgressBar

# Imports
from directories import datadir
from fxns_HELPpayment import HELPpayment, HELPrate
from fxns_globals import read_ATO, low_tsh, indexation_d, indexation_t

###############################################################################
# FILE PARAMETERS
###############################################################################

# Identifiers
idvar = 'SYNTHETIC_AEUID'
tvar = 'INCM_YR'

# Years to create HELP dataset for
firstyr = 2002
lastyr = 2016

# Years of HELP datasets to merge with 2016 Census data
firstC16 = 2016
lastC16 = 2016

###############################################################################
# MAKE DATASET OF IMPORTANT HELP INFORMATION FOR EACH YEAR
###############################################################################

# Read in dataset of debt in 2011
debt11 = read_ATO(2011, 'whlddebt')
debt11['has_debt_2011'] = (debt11['HELOANPGMMECLCTBLDEBTOUTSTGAMT'].fillna(0) > 0).astype(int)
debt11 = debt11[[idvar, 'has_debt_2011']]

# Read in dataset of debt in 2015
debt15 = read_ATO(2015, 'whlddebt')
debt15['has_debt'] = (debt15['HELOANPGMMECLCTBLDEBTOUTSTGAMT'].fillna(0) > 0).astype(int)
debt15 = debt15[[idvar, 'has_debt']]

# Read in demographics file (for sex 1 = male, 2 = female)
file_dem = 'R:\\madipge-ato-d-clientreg-demogs-06-21\\ato_cr_demogs_0621.csv'
demog = dd.read_csv(file_dem, dtype = {'SEX': 'float64', 'SPINE_ID': 'object'})
demog = demog[[idvar, 'SEX', 'DOB_Y']].shuffle(on = idvar).map_partitions(lambda x: x.drop_duplicates(idvar))

# Dictionary of variables to keep and rename
dname = {
    'SEX':'gender',
    'CLNT_RSDNT_IND':'resident',
    'TAXABLE_STATUS':'taxable',
    'IDV_OCPTN_CD':'occupation',
    'SUB_OCPTN_GRP_CD':'occupation',
    'OCPTN4_CD':'occupation',
    'IDV_SEX_CD':'gender',
    'GRS_PMT_TOTL_CALCD_AMT':'wage_b1',
    'GRS_AMT':'wage_b1',
    'ALWNCERNGS_TIPSDRCTRSFEES_AMT':'allowances_b2',
    'AUSNGOVTPNSNS_AND_ALWNC_AMT':'gpayments_b5',
    'AUSNGOVTALWNC_AND_PMTS_AMT':'gpension_b6',
    'GRS_INTST_AMT':'int_b10',
    'DIVS_FRNKD_AMT':'divf_b11',
    'DIVS_UNFRNKD_AMT':'divuf_b11',
    'DIVS_FCR_AMT':'divfcr_b11',
    'CG_NET_AMT':'cg_b18',
    'RNTL_NET_RNT_AMT':'rent_b21',
    'SUPER_CONTRB_AMT':'super_emp',
    'SUPER_CONTR_PNSN_ANTY_AMT':'super_emp',
    'RPRTBL_EMPLYR_SUPER_CNTRBN_AMT':'super_emp',
    'TOTLNOEMPLRSPNTNCNTRBTNDDCTAMT':'super_per',
    'INCM_OR_LSS_TOTL_AMT':'total_income',
    'DDCTNS_TOTL_AMT':'deduc_total',
}

# List of variables to fill missing values with zeros
fillnas = [x for x in dname.values() if '_b' in x or 'super' in x]
fillnas += ['total_income', 'deduc_total']
fillnas = list(set(fillnas))

# Loop to make datasets
for yr in range(firstyr, lastyr + 1):

    ## Post 2011 where you have detailed ATO data
    if yr >= 2011:
        # Specify what variables to keep
        v_context = ['TAXABLE_STATUS', 'IDV_OCPTN_CD']
        v_incloss = ['GRS_PMT_TOTL_CALCD_AMT', 'ALWNCERNGS_TIPSDRCTRSFEES_AMT',
                     'AUSNGOVTPNSNS_AND_ALWNC_AMT', 'AUSNGOVTALWNC_AND_PMTS_AMT',
                     'PRSNL_SRVCS_ATRBD_INCM_AMT', 'GRS_INTST_AMT',
                     'DIVS_FRNKD_AMT', 'DIVS_UNFRNKD_AMT', 'DIVS_FCR_AMT',
                     'PSHPS_NPP_LESS_FORGN_INCM_AMT', 'PSHPS_PP_DSTBN_AMT',
                     'PSI_NET_AMT',
                     'NETINCMORLSS_FRM_BUS_NPP_AMT', 'NET_INCM_OR_LSS_FRM_BUS_PP_AMT',
                     'CG_NET_AMT', 'RNTL_NET_RNT_AMT', 'INCM_OR_LSS_TOTL_AMT',
                     'TRSTS_NPP_LESS_CGFORGNINCM_AMT', 'NONPPFRNKDDSTBNSFRMTRSTSAMT'
                     ]
        v_deduc = ['TOTLNOEMPLRSPNTNCNTRBTNDDCTAMT', 'DDCTNS_TOTL_AMT']
        v_paysum = ['RPRTBL_EMPLYR_SUPER_CNTRBN_AMT']
        # Specify what gets added to taxable income
        adds = ['NRPL_AMT', 'RFBS_TOTL_AMT']
        if yr >= 2006:
            adds += ['EXMT_FORGN_EMPLT_INCM_AMT']
        if yr >= 2010:
            adds += ['NET_FINCL_INVMT_LSS_AMT', 'RPRTBLEMPLR_SPNTN_CNTRBTNS_AMT']
        # Read in dataset that has debt balances, only if they are available.
        if yr <= 2015:
            df = read_ATO(yr, 'whlddebt')
            df = df[[idvar, tvar, 'HELOANPGMMECLCTBLDEBTOUTSTGAMT', 'HE_LOAN_PGMME_RPMT_DUE_AMT']]
            df = df.rename(columns = {
                'HELOANPGMMECLCTBLDEBTOUTSTGAMT':'debt_help',
                'HE_LOAN_PGMME_RPMT_DUE_AMT':'repayment_help'
                })
            df[['debt_help', 'repayment_help']] = df[['debt_help', 'repayment_help']].fillna(0)
            df['has_debt'] = ((df['debt_help'] > 0) & (df['repayment_help'] < df['debt_help'])).astype(int)
            df['debt_remaining_help'] = da.floor((1 + indexation_d[yr])*(df['debt_help'] - df['repayment_help']))
        else:
            df = debt15
        # Read in income-loss dataset
        if yr <= 2019:
            df_il = read_ATO(yr, 'incloss')
        else:
            df_il = read_ATO(yr, 'inc-loss')
        if yr == 2021:
            df_il = df_il.rename(columns = {'Synthetic_AEUID':idvar})
        df_il = df_il[[idvar, tvar, 'TI_OR_LSS_AMT'] + adds + v_incloss]
        # Calculate HELP income and merge
        df_il['TI_OR_LSS_AMT'] = da.maximum(df_il['TI_OR_LSS_AMT'], 0)
        df_il[adds] = df_il[adds].fillna(0)
        df_il['income_help'] = df_il[['TI_OR_LSS_AMT'] + adds].sum(axis = 1)
        # Create labor income and entrepreneurship measures
        df_il['psi_b9'] = df_il['PRSNL_SRVCS_ATRBD_INCM_AMT'].fillna(0)
        df_il['psi_b14'] = df_il['PSI_NET_AMT'].fillna(0)
        df_il['pship_b13'] = df_il[['PSHPS_PP_DSTBN_AMT',
                                    'PSHPS_NPP_LESS_FORGN_INCM_AMT']].fillna(0).sum(axis = 1)
        df_il['solet_b15'] = df_il[['NET_INCM_OR_LSS_FRM_BUS_PP_AMT',
                                    'NETINCMORLSS_FRM_BUS_NPP_AMT']].fillna(0).sum(axis = 1)
        df_il['entrep'] = (np.abs(df_il[['psi_b9', 'pship_b13',
                                         'solet_b15']]).max(axis = 1) > 0).astype(int)
        df_il['labor'] = df_il[['GRS_PMT_TOTL_CALCD_AMT',  # Justin's labor income measure
                                'ALWNCERNGS_TIPSDRCTRSFEES_AMT',
                                'psi_b9', 'psi_b14',
                                'pship_b13', 'solet_b15']].fillna(0).sum(axis = 1)
        # Merge in income-loss dataset and subset to variables you want to keep
        il_keeps = [x for x in v_incloss if x in dname.keys()]
        df_il = df_il[[idvar, 'income_help', 'psi_b9', 'psi_b14', 'pship_b13','solet_b15', 'entrep', 'labor'] + il_keeps]
        df = df.merge(df_il, how = 'inner', on = idvar)
        # Read in context dataset and merge
        df_c = read_ATO(yr, 'context')
        if yr == 2021:
            df_c = df_c.rename(columns = {'Synthetic_AEUID':idvar})
        mvars = ['MDCRE_FULL_LVY_EXMTN_DAYS_CNT', 'MDCRE_HLF_LVY_EXMTN_DAYS_CNT']
        df_c = df_c[[idvar] + mvars + v_context]
        df_c['medicare_exempt'] = (df_c[mvars].fillna(0).sum(axis = 1) > 0).astype(int)
        df_c = df_c[[idvar, 'medicare_exempt'] + v_context]
        df = df.merge(df_c, how = 'left', on = idvar)
        # Read and merge in paysum dataset, removing duplicate observations (which
        # are supposed to occur in this dataset) and summing across remaining observations
        if yr <= 2019:
            df_ps = read_ATO(yr, 'paysum')
        else:
            df_ps = read_ATO(yr, 'pay-sum')
        if yr == 2021:
            df_ps = df_ps.rename(columns = {'Synthetic_AEUID':idvar})
        df_ps = df_ps[[idvar] + v_paysum]
        df_ps = df_ps.shuffle(on = [idvar] + v_paysum).map_partitions(lambda x: x.drop_duplicates([idvar] + v_paysum))
        df_ps = df_ps.groupby(idvar)[v_paysum].sum(split_out = df.npartitions).reset_index()
        df = df.merge(df_ps, how = 'left', on = idvar)
        # Read and merge in deductions dataset
        df_d = read_ATO(yr, 'ded')
        if yr == 2021:
            df_d = df_d.rename(columns = {'Synthetic_AEUID':idvar})
        df_d = df_d[[idvar] + v_deduc]
        df = df.merge(df_d, how = 'left', on = idvar)

    ## Pre 2011 where you have limited ATO data
    else:
        # Specify what variables to keep
        v_context = ['CLNT_RSDNT_IND']
        v_incloss = ['GRS_INTST_AMT']
        v_deduc = ['TOTLNOEMPLRSPNTNCNTRBTNDDCTAMT']
        v_paysum = []
        if yr < 2007:
            v_context += ['SUB_OCPTN_GRP_CD']
        elif yr < 2010:
            v_context += ['OCPTN4_CD']
        else:
            v_context += ['IDV_OCPTN_CD']
        if yr < 2007 or yr > 2009:
            v_incloss += ['GRS_PMT_TOTL_CALCD_AMT']
        else:
            v_paysum = ['GRS_AMT']
        if yr < 2007:
            v_incloss += ['SUPER_CONTRB_AMT']
        elif yr < 2010:
            v_incloss += ['SUPER_CONTR_PNSN_ANTY_AMT']
        # Specify what gets added to taxable income (this is incorrect but limited by data availablity)
        adds = ['zero']
        if yr >= 2007:
            adds += ['RFBS_TOTL_AMT', 'EXMT_FORGN_EMPLT_INCM_AMT']
        # Read in income-loss dataset
        df = read_ATO(yr, 'inc-loss')
        df['zero'] = 0
        df = df[[idvar, tvar, 'TI_OR_LSS_AMT'] + adds + v_incloss]
        # Calculate HELP income
        df['TI_OR_LSS_AMT'] = da.maximum(df['TI_OR_LSS_AMT'], 0)
        df[adds] = df[adds].fillna(0)
        df['income_help'] = df[['TI_OR_LSS_AMT'] + adds].sum(axis = 1)
        df = df[[idvar, tvar, 'income_help'] + v_incloss]
        # Read in context dataset, calculating medicare exempt if available, and merge
        df_c = read_ATO(yr, 'context')
        if yr <= 2006:
            mvars = ['MDCRE_FULL_LVY_EXMTN_DAYS_CNT', 'MDCRE_HLF_LVY_EXMTN_DAYS_CNT']
            df_c = df_c[[idvar] + mvars + v_context]
            df_c['medicare_exempt'] = (df_c[mvars].fillna(0).sum(axis = 1) > 0).astype(int)
            v_context += ['medicare_exempt']
        df_c = df_c[[idvar] + v_context]
        df = df.merge(df_c, how = 'left', on = idvar)
        # Fix problem with resident indicator in 2010 being flipped
        if yr == 2010:
            df['CLNT_RSDNT_IND'] = df['CLNT_RSDNT_IND'].str.replace('N', 'B')
            df['CLNT_RSDNT_IND'] = df['CLNT_RSDNT_IND'].str.replace('Y', 'N')
            df['CLNT_RSDNT_IND'] = df['CLNT_RSDNT_IND'].str.replace('B', 'Y')
        # Merge in debt in 2011
        df = df.merge(debt11, how = 'left', on = idvar)
        # Read and merge in deductions dataset
        df_d = read_ATO(yr, 'ded')[[idvar] + v_deduc]
        df = df.merge(df_d, how = 'left', on = idvar)
        # Read and merge in paysum dataset, removing duplicate observations (which
        # are supposed to occur in this dataset) and summing across remaining observations
        if v_paysum != []:
            df_ps = read_ATO(yr, 'pay-sum')[[idvar] + v_paysum]
            df_ps = df_ps.shuffle(on = [idvar] + v_paysum).map_partitions(lambda x: x.drop_duplicates([idvar] + v_paysum))
            df_ps = df_ps.groupby(idvar)[v_paysum].sum(split_out = df.npartitions).reset_index()
            df = df.merge(df_ps, how = 'left', on = idvar)

    # Merge in demog
    df = df.merge(demog, how = 'left', on = idvar)
    # Final formatting and variable creation
    df = df.rename(columns = dname)
    df['year'] = yr
    df['age'] = yr - df['DOB_Y']
    df = df.drop(columns = ['DOB_Y'])
    df['indexation_d'] = indexation_d[yr]
    df['indexation_t'] = indexation_t[yr]
    df['threshold'] = low_tsh[yr]
    level_04 = np.product([indexation_t[x] for x in range(1991,2005)])
    level_05 = np.product([indexation_t[x] for x in range(1991,2006)])
    level_yr = np.product([indexation_t[x] for x in range(1991,yr + 1)])
    df['tindex_05'] = level_05/level_yr
    # Calculate HELP rates and repayments in current year
    df['rate_help'] = df['income_help'].apply(HELPrate, args = (yr,), meta=('income_help', 'float64'))
    df['reqpay_help'] = df['income_help'].apply(HELPpayment, args = (yr,), meta=('income_help', 'float64'))
    # Calculate the change in HELP rates you'd have experienced in 2004-2005
    # if income was deflated back
    df['income_help_05'] = df['income_help']*df['tindex_05']
    df['income_help_04'] = df['income_help']*level_04/level_yr
    df['rate_help_05'] = df['income_help_05'].apply(HELPrate, args = (2005,), meta=('income_help_05', 'float64'))
    df['rate_help_04'] = df['income_help_04'].apply(HELPrate, args = (2004,), meta=('income_help_04', 'float64'))
    df = df.drop(columns = ['income_help_04', 'income_help_05'])
    # Compute and bring dataset into main memory
    os.chdir(datadir)
    with ProgressBar():
        df = df.compute()
    # Fill missing values
    fills = [x for x in df.columns if x in fillnas]
    df[fills] = df[fills].fillna(0)
    # Format occupation variable
    if 'occupation' in df.columns:
        if df['occupation'].dtype in [float, int]:
            df['occupation'] = df['occupation'].replace(np.nan, 0).astype(int)
        else:
            replaces = {'':0, '?':0, '*':0, np.nan:0, '0.00':0, '* *':0}
            df['occupation'] = df['occupation'].replace(replaces).astype(int)
        df['occupation1'] = df['occupation'].astype(str).apply(lambda x : x[0]).astype(int)
    # Write out dataset
    df.to_stata('help_' + str(yr) + '.dta', write_index = False)
    if yr >= 2011:
        del df, df_il, df_c, df_d, df_ps
    print('Finished year:', yr)

###############################################################################
# MERGE HELP DATASETS WITH 2016 CENSUS
###############################################################################

# Read in ATO-Spine link and format
file_ATO = 'R:\\madipge-ato-c-spine-conc\\ato_v5_spine.csv'
sp_ATO = dd.read_csv(file_ATO)
sp_ATO = sp_ATO.shuffle(on = 'SPINE_ID').map_partitions(
    lambda x: x.drop_duplicates('SPINE_ID', keep = False)
    )

# Read in Spine-Census link and format
file_C16 = 'R:\\madipge-cen16-c-spine-conc\census_2016_v5_spine.csv'
sp_C16 = dd.read_csv(file_C16, dtype =
                     {'DUPLICATE_FLAG':'object',
                      'DUP_CLUSTER_ID':'object'})
sp_C16 = sp_C16[sp_C16['DUPLICATE_FLAG'] != 'Y']
sp_C16 = sp_C16[sp_C16['SPINE_ID'] != '']
sp_C16 = sp_C16.shuffle(on = 'SPINE_ID').map_partitions(
    lambda x: x.drop_duplicates('SPINE_ID', keep = False)
    )
sp_C16 = sp_C16[[idvar, 'SPINE_ID', 'QUALITY']].rename(
    columns = {idvar:'id_C16', 'QUALITY':'match_quality'}
    )

# Inner-join Spines
sp = sp_ATO.merge(sp_C16, on = 'SPINE_ID')

# List of variables from Census datasets
v_person = ['HRSP', 'HEAP', 'QALFP', 'QALLP']
v_dwel = ['TEND', 'MRED', 'RNTD', 'VEHD']

# Read in Census person-level data
f_person = 'R:\\madip-ge-030101d-census2016-census2016person-2016\madip-ge-030101d-census2016-census2016person-2016\census_2016_person.csv'
c16_person = dd.read_csv(f_person)[[idvar] + v_person].rename(columns = {idvar:'id_C16'})

# Read in Census dwelling-level data and merge with spine
f_dwel = 'R:\\madip-ge-030103d-census2016-census2016dwelling-2016\madip-ge-030103d-census2016-census2016dwelling-2016\census_2016_dwelling.csv'
c16_dwel = dd.read_csv(f_dwel)[[idvar] + v_dwel].rename(columns = {idvar:'id_C16'})

# Merge Census data together and then merge with spine
c16 = c16_person.merge(c16_dwel, on = 'id_C16')
c16 = c16.merge(sp, on = 'id_C16').drop(columns = ['id_C16', 'SPINE_ID'])

# Loop through years merging HELP data with Census data
for yr in range(firstC16, lastC16 + 1):
    os.chdir(datadir)
    df = pd.read_stata('help_{}.dta'.format(yr))
    ddf = dd.from_pandas(df, npartitions = 10)
    ddf = ddf.merge(c16, on = idvar, how = 'left')
    with ProgressBar():
        df = ddf.compute()
    df.to_stata('help_' + str(yr) + '.dta', write_index = False)
    del df, ddf
    print('Finished Census merge for year:', yr)
