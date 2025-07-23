import os
import numpy as np
import dask.dataframe as dd


###############################################################################
# GLOBAL VARIABLES
###############################################################################

# HELP balance indexation rates from atotaxrates.info
indexation_d = {
    1990:1.08,
    1991:1.064,
    1992:1.024,
    1993:1.009,
    1994:1.019,
    1995:1.025,
    1996:1.016,
    1997:1.02,
    1998:0.999,
    1999:1.019,
    2000:1.019,
    2001:1.053,
    2002:1.036,
    2003:1.031,
    2004:1.024,
    2005:1.024,
    2006:1.028,
    2007:1.034,
    2008:1.028,
    2009:1.039,
    2010:1.019,
    2011:1.03,
    2012:1.029,
    2013:1.020,
    2014:1.026,
    2015:1.021,
    2016:1.015,
    2017:1.015,
    2018:1.019,
    2019:1.018,
    2020:1.018,
    2021:1.006,
    2022:1.039
}

# Lowest HELP threshold in each year
low_tsh = {
    1990:23582,
    1991:25468,
    1992:27097,
    1993:27747,
    1994:26402,
    1995:26852,
    1996:27674,
    1997:28494,
    1998:20700,
    1999:21333,
    2000:21983,
    2001:22345,
    2002:23241,
    2003:24364,
    2004:25347,
    2005:35000,
    2006:36184,
    2007:38148,
    2008:39824,
    2009:41598,
    2010:43151,
    2011:44911,
    2012:47195,
    2013:49095,
    2014:51308,
    2015:53345,
    2016:54125,
    2017:54868,
    2018:55873,
    2019:51956,
    2020:45880,
    2021:46619
}

# HELP threshold indexation rates from HECS-HELP history file
# Note: I set this manually based on AWOTE in years of a policy change,
# which is an approximation because threshold growth isn't always
# equal to AWOTE but it is close
indexation_t = {}
for yr in range(1991,2022):
    indexation_t[yr] = low_tsh[yr]/low_tsh[yr - 1]
indexation_t[1998] = 37344/35942
indexation_t[2005] = 50929/48571
indexation_t[2019] = 84248/82027
indexation_t[2020] = 87680/84248

# Minimum wages: source = Wikipedia
min_wage = {
    1991:5.64,
    1992:6.59,
    1993:6.59,
    1994:6.80,
    1995:7.01,
    1996:7.36,
    1997:7.38,
    1998:9.45,
    1999:9.82,
    2000:10.14,
    2001:10.53,
    2002:10.87,
    2003:11.35,
    2004:11.80,
    2005:12.30,
    2006:12.75,
    2007:13.37,
    2008:13.74,
    2009:14.31,
    2010:14.31,
    2011:15.00,
    2012:15.41,
    2013:15.96,
    2014:16.37,
    2015:16.87,
    2016:17.29,
    2017:17.70,
    2018:18.29,
    2019:18.93,
    2020:19.49,
    2021:19.84
}



###############################################################################
# FUNCTIONS
###############################################################################

# Function for clipping outliers on percentiles
def clip_outlier(df_in, cols, p_low = 0.02, p_high = 0.98):
    df = df_in.copy()
    if isinstance(cols, str):
        cols = [cols]
    winsordata = df[cols]
    df[cols] = winsordata.clip(lower=winsordata.quantile(p_low),
           upper=winsordata.quantile(p_high), axis=1)
    return df

# Functions for calculating dominance statistics
def dominance1_mean(x):
    total = np.sum(x)
    max = np.max(x)
    return max / total
def dominance2_mean(x):
    total = np.sum(x)
    max1 = np.max(x)
    max2 = np.max(x[x < max1])
    return (max1 + max2) / total

# Function to read in different ATO datasets
def read_ATO(year, name):
    # List ATO datasets in R: folder
    if year >= 2011 and year <= 2019:
        files = [x for x in os.listdir('R:\\') if 'madip-ge' in x and '-ato-' in x]
    else:
        files = [x for x in os.listdir('R:\\') if 'madipge' in x and '-ato-' in x]
    # Read in dataset of interest
    folder = [x for x in files if x[-2:] == str(year)[-2:] and name in x][0] + '\\'
    dfdir = 'R:\\' + folder
    if year >= 2011 and year <= 2016:
        dfdir += folder
    elif year == 2019 and name != 'paysum':
        dfdir += folder
    os.chdir(dfdir)
    csvfile = [x for x in os.listdir(os.getcwd()) if '.csv' in x][0]
    force_types = {
        'INTRNT_USE_TO_SELL_GS_IND': 'object',
        'MN_BUS_TAX_OFC_ANZSIC_CD': 'float64',
        'WRK_RLTD_SELF_EDUCN_TYP_CD': 'object',
        'IMPUTN_CR_AMT': 'float64',
        'INCM_OR_LSS_TOTL_AMT': 'float64',
        'SUPER_CONTRB_AMT': 'float64',
        'TOTL_RBTS_AMT': 'float64',
        'BUS_NET_TAX_AMT': 'float64',
        'LSPS_AMT_A_TW_TOTL_CALCD_AMT': 'float64',
        'MLS_CALCD_AMT': 'float64',
        'SW_TW_TOTL_CALCD_AMT': 'float64',
        'LSPS_AMT_B_TW_TOTL_CALCD_AMT': 'float64',
        'MLS_NOT_PYBL_DAYS_CNT': 'float64',
        'PSI_80_PCT_ONE_SRC_IND': 'object',
        'PSI_BUS_PRMS_TST_IND': 'object',
        'PSI_EMPLT_TST_IND': 'object',
        'PSI_UNRLTD_CLNTS_TST_IND': 'object',
        'PNSN_GOVTALWANCE_OFSTCALCD_AMT': 'float64',
        'UNPLMT_SKNS_BNFT_RBT_AMT': 'float64',
        'P_T_IMPUTN_CR_AMT': 'float64',
        'CLOSG_STK_ACTN_CD': 'object',
        'DPRCTN_EXPNS_CD': 'object',
        'OCPTN4_CD': 'object',
        'OTHR_BUS_INCM_NPP_CD': 'object',
        'OTHR_BUS_INCM_PP_CD': 'object',
        'PNSNR_TAX_OFST_CD': 'object',
        'PNSNR_VETERAN_CD': 'object',
        'SNR_AUSNS_VETERAN_CD': 'object',
        'TRDG_STK_ELCTN_IND': 'object',
        'ASSBL_GOVT_INDY_PMTS_NPP_CD': 'object',
        'ASSBL_GOVT_INDY_PMTS_PP_CD': 'object',
        'TI_OR_LSS_AMT': 'float64',
        'ETP_DT': 'object',
        'IDV_SEX_DCD': 'float64',
        'PRT_YR_TFT_DT': 'object',
        'SLS_PMT_DT': 'object',
        'AGE0910_END': 'float64',
        'AGE0910_START': 'float64',
        'GRS_PMT_TOTL_CALCD_AMT': 'float64',
        'TOTL_INCMORLSS_LESS_DDCTNS_AMT': 'float64',
        'NET_FINCL_INVMT_LSS_AMT': 'float64',
        'NRPL_AMT': 'float64',
        'RFBS_TOTL_AMT': 'float64',
        'RPRTBLEMPLR_SPNTN_CNTRBTNS_AMT': 'float64',
        'AGE_FY_START': 'float64',
        'SPS_OR_HSKPR_TAX_OFST_CLM_CD': 'object',
        'SLRY_AND_WG_EXPNSS_TOTL_CD': 'object',
        'IDV_OCPTN_CD': 'object',
        'AGE_PNSN_ACTN_CD': 'object',
        'INTST_IN_FORGN_COY_OR_TRST_IND': 'object',
        'OCPTN_GRP_CD': 'object',
        'SUB_OCPTN_GRP_CD': 'object',
        'SUPER_CONTRB_ACTN_CD': 'object',
        'LSPA_TYP': 'object',
        'PERD_PMT_DT': 'object',
        'PSI_BUS_DTMNTN_IND': 'object',
        'RSLTS_TST_IND': 'object',
        'SPS_DIE_DRG_YR_IND': 'object',
        'HLTH_FND_CD_3': 'object',
        'HLTH_FND_CD_4': 'object',
        'XMEMPTYPENFP': 'object',
        'PMT_DT': 'object'
        }
    df = dd.read_csv(csvfile, dtype = force_types, assume_missing = True)
    return df
