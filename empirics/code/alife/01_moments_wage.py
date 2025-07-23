import os
import gc
import pandas as pd
import numpy as np
import statsmodels.api as sm
from scipy.stats import norm

# Imports
from directories import taxdir_dta, file_debt, momdir, sedir
from fxns_globals import clip_outlier, series_txt, indexation_t, min_wage
from fxns_bootstrap import bootstrap_vector

###############################################################################
# FILE PARAMETERS
###############################################################################

# Years of data to look at
firstyr = 1991
lastyr = 2019

# Age filter and minimum cohort
min_age = 22
max_age = 64
min_cohort = 1963

# Income variable
inc_var = 'i_salary_wage'

# Unique identifier
idvar = 'alife_id_001'

# Number of bootstraps for moments that don't have analytical formulas
Nb = 100

###############################################################################
# MAKE POOLED DATASET OF INCOME
###############################################################################

# Function to adjust for inflation to 2005 dollars using HELP threshold indexation
def adjust_infl(y, year):
    level_05 = np.product([indexation_t[x] for x in range(1991,2006)])
    level_yr = np.product([indexation_t[x] for x in range(1991, year + 1)])
    return y * level_05 / level_yr

# Stack datasets
dfs = []
for yr in range(firstyr, lastyr + 1):
    # Read in data
    os.chdir(taxdir_dta)
    data = pd.read_stata('aLife10_tax_' + str(yr) + '.dta')
    data['year'] = yr
    # Filters
    data = data[(data['c_age_30_june'] >= min_age) & (data['c_age_30_june'] <= max_age)]
    data = data[data['c_self_emp_flag'] == 0]
    if 'isn_pt_trust_dist_npp_cd' in data.columns:
        data = data[~data['isn_pt_trust_dist_npp_cd'].isin(['S', 'T', 'I'])]
    data[inc_var] = data[inc_var].mask(data[inc_var] < 0.5*38*13*min_wage[yr])
    # Winsorize income at 99.999
    data = clip_outlier(data, inc_var, p_low = 0.0, p_high = 0.99999)
    # Adjust for inflation
    data[inc_var] = data[inc_var].apply(adjust_infl, args = (yr,))
    # Keep only columns that you need
    data['log_wage'] = np.log(data[inc_var])
    data['age'] = data['c_age_30_june']
    data = data[[idvar, 'year', 'log_wage', 'age']].dropna()
    # Append
    dfs.append(data)
    del data
    print('Finished year:', yr)
    
# Concatenate
df = pd.concat(dfs, ignore_index = True).sort_values([idvar, 'year']).reset_index()
del dfs
    
# Read in debt balances
debtbals = pd.read_csv(file_debt)
debtbals[idvar] = debtbals[idvar].astype(float) # convert to format of aLife10_tax files
debtbals = debtbals.rename(columns = {'help_debt_bal':'debt_balance', 'income_year':'year'})
debtbals = debtbals[(debtbals['year'] >= firstyr) & (debtbals['year'] <= lastyr)]
# Trim few # of negative balance observations
debtbals['debt_balance'] = np.maximum(debtbals['debt_balance'], 0)
# Adjust for inflation
debtbals['debt_balance'] = debtbals.apply(
        lambda x: adjust_infl(x['debt_balance'], int(x['year'])),
        axis = 1
)
# Merge and fill missing values with zero
df = df.merge(debtbals, on = [idvar, 'year'], how = 'left')
df['debt_balance'] = df['debt_balance'].fillna(0)     
# Calculate positive debt balance dummy
df['has_debt'] = (df['debt_balance'] > 0).astype(int)

# Merge in whether people had debt at minimum age
debt_min = df[df['age'] == min_age][[idvar, 'debt_balance']].copy()
debt_min = debt_min.rename(columns = {'debt_balance':'debt_initial'})
debt_min['has_debt_initial'] = (debt_min['debt_initial'] > 0).astype(int)
df = df.merge(debt_min, on = idvar, how = 'left')
df[['debt_initial', 'has_debt_initial']] = df[['debt_initial', 'has_debt_initial']].fillna(0)

# Sort
df = df.sort_values([idvar, 'year'])

# Clean up
del debtbals, debt_min
gc.collect()

###############################################################################
# CALCULATE TARGETED MOMENTS
###############################################################################

# Calculate cohort and filter
df['cohort'] = (df['year'] - df['age'] + min_age).astype(int)
df = df.query('cohort >= @min_cohort')

# Ages for cross-sectional variances
ages = [22, 32, 42, 52, 62]

# Percentiles for income growth
percentiles = [10, 50, 90]

# Average income level
wage_series = np.exp(df['log_wage'])
series_txt(pd.Series(wage_series.mean()), f'{momdir}/mean_income.txt')
series_txt(pd.Series(wage_series.std() / np.sqrt(len(wage_series))), f'{sedir}/mean_income.txt')
del wage_series
gc.collect()

# Cross sectional-variance of income at various ages
base_c = df.groupby('cohort')['log_wage'].count().idxmax()
print('Base cohort for X/S variances:', base_c)
var_as = df.groupby(['age', 'cohort'])['log_wage'].var().reset_index()
var_as['age'] = var_as['age'].astype(int)
var_as = var_as.merge(pd.get_dummies(var_as['cohort'], prefix = 'cohort'), 
                      left_index = True, right_index = True)
var_as = var_as.merge(pd.get_dummies(var_as['age'], prefix = 'age'), 
                      left_index = True, right_index = True)
dummies_a = [x for x in var_as.columns if 'age_' in x]
dummies = dummies_a + [x for x in var_as.columns if 'cohort_' in x and str(base_c) not in x]
model = sm.OLS(var_as['log_wage'], var_as[dummies]).fit(cov_type = 'HC0')
model.params[dummies_a].plot()
series_txt(model.params[dummies_a][['age_' + str(a) for a in ages]], f'{momdir}/cs_variances.txt')
series_txt(model.HC0_se[dummies_a][['age_' + str(a) for a in ages]], f'{sedir}/cs_variances.txt')
gc.collect()

# Second-order polynomial in age and remove from wages, with cohort fixed effects
df['age2'] = df['age']**2
df = df.merge(pd.get_dummies(df['cohort'], prefix = 'cohort'), left_index = True, right_index = True)
Xs = ['age', 'age2'] + [x for x in df.columns if 'cohort_' in x]
model = sm.OLS(df['log_wage'], df[Xs]).fit(cov_type = 'HC0')
series_txt(model.params[['age', 'age2']], f'{momdir}/age_effects.txt')
series_txt(model.HC0_se[['age', 'age2']], f'{sedir}/age_effects.txt')
df['log_wage_res'] = model.resid
gc.collect()

# Calculate autocorrelation of income at different horizons
df = df.sort_values([idvar, 'year'])
df['l1_res'] = df.groupby(idvar)['log_wage_res'].shift(1)
df['l5_res'] = df.groupby(idvar)['log_wage_res'].shift(5)
def calc_ar1(data):
    return np.array([data[['log_wage_res', 'l1_res']].cov().iloc[0,1],
                     data[['log_wage_res', 'l5_res']].cov().iloc[0,1]])
series_txt(pd.Series(calc_ar1(df)), f'{momdir}/autocovariance.txt')
series_txt(pd.Series(bootstrap_vector(df[['log_wage_res', 'l1_res', 'l5_res']], calc_ar1, Nb)), 
           f'{sedir}/autocovariance.txt')
gc.collect()

# Calculate average percentiles of 1 and 5-year income growth RESIDUALS
df['1y_growth'] = df['log_wage_res'] - df['l1_res']
df['5y_growth'] = df['log_wage_res'] - df['l5_res']
for p in percentiles:
    def mean_p(data):
        return data.groupby('year')[['1y_growth', '5y_growth']].quantile(p/100).mean().values
    series_txt(pd.Series(mean_p(df)), f'{momdir}/p{p}_growth.txt')
    series_txt(pd.Series(bootstrap_vector(df[['year', '1y_growth', '5y_growth']], mean_p, Nb)), 
               f'{sedir}/p{p}_growth.txt')
    gc.collect()
    
# Calculate kurtosis of 1 and 5-year income growth RESIDUALS
def calc_kurt(data):
    return data.groupby('year')[['1y_growth', '5y_growth']].apply(pd.DataFrame.kurt).mean().values
series_txt(pd.Series(calc_kurt(df)), f'{momdir}/kurt_growth.txt')
series_txt(pd.Series(bootstrap_vector(df[['year', '1y_growth', '5y_growth']], calc_kurt, Nb)), 
           f'{sedir}/kurt_growth.txt')
gc.collect()

# Regression to estimate difference in age profile among subset of individuals
# who we see at min age between those who have debt and those who don't
df['min_age'] = df.groupby(idvar)['age'].transform(min)
df_ma = df[df['min_age'] == min_age].copy()
df_ma['age_d'] = df_ma['age'] * df_ma['has_debt_initial']
Xs = ['age', 'age2', 'has_debt_initial', 'age_d'] + [x for x in df.columns if 'cohort_' in x]
model_d = sm.OLS(df_ma['log_wage'], df_ma[Xs]).fit(cov_type = 'HC0')
series_txt(model_d.params[['has_debt_initial', 'age_d']], f'{momdir}/debt_premium.txt')
series_txt(model_d.HC0_se[['has_debt_initial', 'age_d']], f'{sedir}/debt_premium.txt')
gc.collect()

# Calculate average income at oldest age you can observe among has_initial_debt = 1
maxD_age = df_ma['age'].max()
print(f'Maximum age of individuals for whom you can identify has_debt: {maxD_age}')
model_l = sm.OLS(df_ma['log_wage'], df_ma[[x for x in df.columns if 'cohort_' in x]]).fit(cov_type = 'HC0')
df_ma['wage_cfe'] = np.exp(model_l.resid + df_ma['log_wage'].mean())
df_ma_aD = df_ma[df_ma['age'] == maxD_age].query('has_debt_initial == 1')['wage_cfe'].copy()
series_txt(pd.Series(df_ma_aD.mean()), f'{momdir}/mean_income_aD.txt')
series_txt(pd.Series(df_ma_aD.std() / np.sqrt(len(df_ma_aD))), f'{sedir}/mean_income_aD.txt')
del df_ma_aD
gc.collect()

###############################################################################
# CALCULATE MOMENTS THAT CONTROL INTIAL DEBT DISTRIBUTION
###############################################################################

# Calculate fraction of people with debt at min_age
p_e = df_ma[df_ma['age'] == min_age]['has_debt'].mean()

# Calculate max, mean, and variance of log initial debt
debt0 = df_ma[df_ma['age'] == min_age]['debt_balance']
max_d = np.max(debt0)
debt0 = np.log(debt0[debt0 > 100])
mu_d, sigma_d = norm.fit(np.array(debt0))

# Output in one file
series_txt(pd.Series([p_e, mu_d, sigma_d, max_d]), f'{momdir}/debt0_distribution.txt')

###############################################################################
# CALCULATE DISTRIBUTION OF AGES AT THE TIME OF 2005 POLICY CHANGE
###############################################################################

df05 = df[df['year'] == 2005].groupby('age')[idvar].count()
ages_05 = df05/df05.sum()
series_txt(ages_05, f'{momdir}/age_distribution_2005.txt')

###############################################################################
# CALCULATE DISTRIBUTION OF YEARS AT WHICH PEOPLE ARE min_age
###############################################################################

births = df.drop_duplicates(subset = [idvar, 'cohort']).groupby('cohort')[idvar].count().sort_index()
births /= births.sum()
series_txt(births, f'{momdir}/birth_cohorts.txt')
