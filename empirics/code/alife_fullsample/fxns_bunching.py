# Module imports
import pandas as pd
import numpy as np
from patsy import dmatrix
import statsmodels.api as sm
import matplotlib.pyplot as plt

# Set seed for bootstrapping
np.random.seed(123456789)

# Cycler for plots
from cycler import cycler
cyc = (cycler(color = ['darkblue', 'maroon', 'darkgreen', 'darkorange']*3) +
       cycler(linestyle = ['solid', 'dashed', 'dashdot']*4))
plt.rc('axes', prop_cycle = cyc)

def bin_df(data_in, var, low, high, binw, tsh):
    '''
    This function rounds a variable within a dataframe into bins of
    size binw, centering around a threshold tsh. It then filters to
    bins close to the threshold based on low and high.

    Parameters
    ----------
    data_in : dataframe to round that contains var
    var : name of variable you want to get empirical density of
    tsh : numeric value that you want to center bins around such that one
        bin starts at var = tsh
    low : lowest value of var you want to consider
    high : highest value of var you want to consider
    binw : width of bins for empirical density

    Returns
    -------
    data : dataframe with var rounded and sample filtered
    tsh_r : rounded threshold

    '''
    # Copy
    data = data_in.copy()
    # Subtract an additional 1 so that people at 0 are first non-bunchers
    data[var] = data[var] - tsh - 1
    # Round income variable and threshold
    data[var] = binw * np.round(np.floor(data[var] / binw))
    tsh_r = binw * np.round(np.floor(tsh / binw))
    # Recenter with rounded threshold
    data[var] += tsh_r
    # Filter to people close to the treshold
    data = data[(data[var] >= low) & (data[var] <= high)]
    return data, tsh_r

def empirical_density(data, idvar, var, tsh, low, high, binw):
    '''
    This function calculates the empirical density in counts and percent
    of observations in bins, where the bins are set such that one bin starts
    at var = tsh.

    Parameters
    ----------
    data : input pandas dataframe that must contain at least two variables:
        idvar and var
    idvar : name of variable that uniquely identifies an observation (doesnt
        actually have to be, just needs to be a variable to count)
    var : name of variable you want to get empirical density of
    tsh : numeric value that you want to center bins around such that one
        bin starts at var = tsh
    low : lowest value of var you want to consider
    high : highest value of var you want to consider
    binw : width of bins for empirical density

    Returns
    -------
    den : pandas dataframe containing empirical counts and percent of observations
        within each bin
    norm : total number of observations considered
    tsh_r : tsh rounded to nearest bin

    '''
    # Bin data around threshold
    data, tsh_r = bin_df(data, var, low, high, binw, tsh)
    # Calculate counts and percentages by groups
    den = data.groupby(var)[idvar].count()
    print('Minimum bin size:', den.min())
    norm = den.sum()
    den = pd.DataFrame({'count':den, 'percent':den/norm*100}).reset_index()
    return den, norm, tsh_r

def bstat_heterogeneity(data, idvar, var, tsh, lowY, highY, binw,
                       hvar, values, bins_below, method, dof,
                       denvar = 'percent'):
    # Loop through values
    b = []
    for i in range(len(values)):
        # Subset data
        data1 = data[data[hvar] == values[i]].copy()
        # Empirical densities
        f1 = empirical_density(data1, idvar, var, tsh, lowY, highY, binw)[0]
        # Calculate bunching statistic and standard error
        output = cfdensity_tsh_iter_bs(f1, binw, bins_below, tsh, method,
                                       dof, var, denvar, 0)
        b.append(output[1])
    return b

def create_features(df_in, var, method, dof):
    '''
    This function creates splines or polynomials (including intercept) of
    a variable var in df_in and then returns df_in with these features attached.

    Parameters
    ----------
    df_in : pandas DataFrame containing var
    var : variable to take transformations of
    method : = 'spline' or 'polynomial'
    dof : degrees of freedom in splines, which = # knots - 1, or degree of polynomial

    Returns
    -------
    df : pandas DataFrame just like df_in, except with features added

    '''
    df = df_in.copy()
    if method == 'spline':
        splines = dmatrix("cr(train,df=" + str(dof) + ")", {"train":df[var]},
                          return_type = 'dataframe')
        splines.columns = ['spline' + str(i) for i in range(dof + 1)]
        df = df.merge(splines, left_index = True, right_index = True)
    elif method == 'polynomial':
        df['poly0'] = 1
        for p in range(1, dof + 1):
            df['poly' + str(p)] = df[var]**p
    return df

def cfdensity_tsh(df_in, binw, n_below, n_above, tsh, method, dof, var, denvar):
    '''
    This function estimates the counterfactual density around one threshold.
    It works by fitting a spline or polynomial to the empirical density
    with separate individual dummies for each bin within n_below (n_above) bins
    below (above) the threshold, tsh.

    Parameters
    ----------
    df_in : pandas DataFrame that is the empirical density output from
        empirical_density()
    binw : width of bins for empirical density
    n_below : number of observations below the threshold to exclude
    n_above : number of observations above the threshold to exclude
    tsh : location of threshold
    method : = 'spline' or 'polynomial'
    dof : degrees of freedom in splines, which = # knots - 1, or degree of polynomial
    var : column name of continuous variable which empirical density is measured around
    denvar : column name of density variable

    Returns
    -------
    df : pandas DataFrame like df_in, but with results including counterfactual
        density labeled 'cf_' + denvar
    bstat : bunching mass statistic = excess mass below / CF density below
    error : integration error

    '''
    # Copy
    df = df_in.copy()
    # Define bunching windows below and above
    df['below'] = ((df[var] < tsh) & (df[var] >= tsh - binw * n_below)).astype(int)
    df['above'] = ((df[var] >= tsh) & (df[var] <= tsh + binw * (n_above - 1))).astype(int)
    # Create individual dummies for each bin below and above
    for v in ['below', 'above']:
        df[v + '_j'] = (df[var] * df[v]).astype(int).astype(str)
        df = pd.get_dummies(df, columns = [v + '_j'], prefix = v, drop_first = True)
    # Create splines or polynomials (includes intercept) and merge to dataset
    df = create_features(df, var, method, dof)
    # Run regression
    Xs = [x for x in df.columns if 'spline' in x or 'poly' in x]
    cf_Xs = [x for x in df.columns if 'below_' in x or 'above_' in x]
    fit = sm.OLS(df[denvar], df[Xs + cf_Xs]).fit()
    # Store residuals in case you bootstrap later
    df['resid_' + denvar] = df[denvar] - fit.fittedvalues
    # Calculate counterfactual density
    df['cf_' + denvar] = fit.fittedvalues
    for x in cf_Xs:
        df['cf_' + denvar] -= fit.params[x] * df[x]
    # Calculate excess mass above and below threshold
    below = 0
    above = 0
    for x in cf_Xs:
        if 'below' in x:
            below += fit.params[x]
        elif 'above' in x:
            above += fit.params[x]
    # Calculate bunching statistic: excess mass as % of CF density
    ## Note: Chetty et al. (2011) do .mean() here instead of .sum(), not sure why,
    ## and they use average over both 'below' = 1 and 'above' = 1
    cf_below = df[df['below'] == 1]['cf_' + denvar].sum()
    #cf_below = df[df['below'] + df['above'] > 0]['cf_' + denvar].mean() # Chetty statistic
    bstat = below / cf_below
    # Error in integration constraint
    error = (below + above)/max(abs(below), 0.001)
    # Return
    return df, bstat, error

def cfdensity_tsh_iter(df, binw, n_below, tsh, method, dof, var, denvar):
    '''
    This function is a wrapper to cfdensity_tsh() that applies the method from
    Kleven-Waseem (2013) to determine the number of points above the threshold
    that should be excluded from the estimation above the threshold. It does
    this by repeatedly applying cfdensity_tsh() with n_above = 1 to max_above,
    where max_above is set equal to half of the number of bins. It then
    chooses the value of n_above that minimizes the integration error, given
    that the extra mass above the threshold must be equal to the missing mass
    below in a counterfactual.
    '''
    # Loop over bins_above to figure out which value satisfies integration constraint
    max_above = np.floor(len(df)/2).astype(int) - 1 # maximum number bins above
    list_nabove = list(range(1, max_above))
    res = [cfdensity_tsh(df, binw, n_below, x, tsh, method, dof, var, denvar)[-1]
            for x in list_nabove]
    # Take minimum error value and re-run things at that
    bins_above = list_nabove[np.argmin([np.abs(x) for x in res])]
    df, bstat, error = cfdensity_tsh(df, binw, n_below, bins_above, tsh, method,
                          dof, var, denvar)
    return df, bstat, error, bins_above, n_below

def cfdensity_tsh_iter_bs(df_in, binw, n_below, tsh, method, dof, var, denvar,
                          Nb = 100):
    '''
    This function is a wrapper for cfdensity_tsh_iter() that evaluates it
    and then performs a bootstrap following Chetty et al. (2011) to calculate
    the standard error of the bunching mass statistic.

    Nb : number of bootstraps, set to zero for no bootstrap
    '''
    # Estimate baseline
    df, bstat, error, bins_above, bins_below = cfdensity_tsh_iter(df_in, binw, n_below, tsh,
                                                      method, dof, var, denvar)
    # Perform bootstrap
    resids = np.array(df['resid_' + denvar])
    bstat_bs = []
    if Nb > 0:
        for i in range(Nb):
            df_bs = df_in.copy()
            df_bs[denvar] += np.random.choice(resids, size = len(df), replace = True)
            bstat_bs.append(cfdensity_tsh_iter(df_bs, binw, n_below, tsh,
                                               method, dof, var, denvar)[1])
        se = np.std(bstat_bs)
    else:
        se = 0
    return df, bstat, error, bins_above, bins_below, se
