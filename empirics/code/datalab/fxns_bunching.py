import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import FixedLocator

# Imports
from directories import figdir, tabledir
from fxns_globals import dominance1_mean, dominance2_mean

# Set seed for bootstrapping
np.random.seed(123456789)

# Cycler for plots
from cycler import cycler
cyc = (cycler(color = ['darkblue', 'maroon', 'darkgreen', 'darkorange']) +
       cycler(linestyle = ['-', '--', ':', '-.']))
plt.rc('axes', prop_cycle = cyc)

def remove_outlier(df_in, cols, n_iqr = 5):
    df = df_in.copy()
    if isinstance(cols, str):
        cols = [cols]
    for col_name in cols:
        q1 = df[col_name].quantile(0.25)
        q2 = df[col_name].quantile(0.50)
        q3 = df[col_name].quantile(0.75)
        iqr = q3-q1 #Interquartile range
        fence_low  = q2-n_iqr*iqr
        fence_high = q2+n_iqr*iqr
        df = df.loc[(df[col_name] > fence_low) & (df[col_name] < fence_high)]
    return df

def clip_outlier_iqr(df_in, cols, n_iqr = 5):
    df = df_in.copy()
    if isinstance(cols, str):
        cols = [cols]
    q1 = df[cols].quantile(0.25)
    q2 = df[cols].quantile(0.5)
    q3 = df[cols].quantile(0.75)
    mins = q2 - n_iqr * (q3-q1)
    maxs = q2 + n_iqr * (q3-q1)
    df[cols] = df[cols].clip(mins, maxs, axis=1)
    return df

def clip_outlier(df_in, cols, p_low = 0.02, p_high = 0.98):
    df = df_in.copy()
    if isinstance(cols, str):
        cols = [cols]
    winsordata = df[cols]
    df[cols] = winsordata.clip(lower=winsordata.quantile(p_low), 
           upper=winsordata.quantile(p_high), axis=1)
    return df

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

def binscatter_bunch(data_in, yr1, yr2, idvar, xvar, yvar, ylab, tsh, lowY, highY, binw,
                     close = False, cval = 1.96, denvar = 'percent', ytype = 'int',
                     name = ''):
    '''
    This function plots the average value of yvar in bins of xvar, where xvar
    is a variable that exhibits bunching in its distribution and a threshold,
    tsh.

    Parameters
    ----------
    data_in : underlying data
    yr1 : first yr of data for plot label
    yr2 : last yr of data for plot label
    idvar : name of variable that uniquely identifies an observation (doesnt
        actually have to be, just needs to be a variable to count)
    xvar : variable that will be on x-axis that you have bunching in
    yvar : variable that will be on y-axis that you will take mean of
        within each bin of xvar
    ylab : label of y variable to plot
    tsh : threshold location
    lowY : lowest value of xvar to consider
    highY : highest value of xvar to consider
    binw : bin width of xvar
    close : whether to close plot
    cval : critical value for confidence intervals set to zero for simple
        line plot
    denvar : column name of density variable ('count' or 'percent')
    ytype : type of y variable to format y-axes ('int' or 'float')
    name : additional test to add to file name

    '''
    # Determine filename
    filename = 'bunching_binscatter_{}_{}{}{}'.format(yvar, yr1, yr2, name)
    # Drop missing observations
    data = data_in.dropna(subset = [yvar]).copy()
    if len(data) != len(data_in):
        print('{}% missing for {}'.format(100*(1-len(data)/len(data_in)), yvar))
    # Calculate empirical density of xvar, output counts, and plot
    f = empirical_density(data, idvar, xvar, tsh, lowY, highY, binw)[0]
    os.chdir(tabledir)
    f[[xvar, 'count']].to_csv(filename + '.csv', index = False)
    fig, axes = plt.subplots(1,1)
    f.plot(ax = axes, x = xvar, y = denvar, kind = 'line',
           legend = False, color = 'maroon', alpha = 0.3)
    # Bin data and take group by means and output dominance counts
    data, tsh_r = bin_df(data, xvar, lowY, highY, binw, tsh)
    meany = data.groupby(xvar)[yvar].mean().reset_index()
    data.groupby(xvar)[yvar].apply(dominance1_mean).reset_index().to_csv(f'{filename}_dominance1.csv', index = False)
    data.groupby(xvar)[yvar].apply(dominance2_mean).reset_index().to_csv(f'{filename}_dominance2.csv', index = False)
    sery = cval * data.groupby(xvar)[yvar].sem().reset_index()
    # Plot on secondary y-axis
    axes2 = axes.twinx()
    if cval > 0:
        axes2.errorbar(meany[xvar], meany[yvar], sery[yvar], fmt = 'o',
                       color = 'darkblue', alpha = 0.7, capsize = 4)
    else:
        meany.plot(ax = axes2, x = xvar, y = yvar, kind = 'line', marker = 'o',
               legend = False, color = 'darkblue', alpha = 0.5)
    # Final plot formatting and save
    axes.axvline(tsh_r, color ='black', alpha = 0.8)
    axes.set_xlabel('HELP Income Relative to 0% Threshold')
    axes.set_ylabel('% of Individuals')
    axes.set_title(year_label(yr1, yr2))
    format_ticks_dp(axes)
    axes.set_ylim([f[denvar].min()*0.9, f[denvar].max()*1.1])
    yticks = axes2.get_yticks()
    axes2.yaxis.set_major_locator(FixedLocator(yticks))
    if ytype == 'int':
        axes2.set_yticklabels(['{:,}'.format(int(x)) for x in yticks])
    elif ytype == 'float':
        axes2.set_yticklabels(['{:.2f}'.format(x) for x in yticks])
    axes2.set_ylabel(ylab)
    os.chdir(figdir)
    plt.savefig(filename + '.pdf', bbox_inches = 'tight')
    if close:
        plt.close()

def year_label(yr1, yr2):
    if yr1 == yr2:
        lab = '{}'.format(yr1)
    else:
        lab = '{} to {}'.format(yr1, yr2)
    return lab

def format_ticks_dp(axes):
    xticks = axes.get_xticks()
    yticks = axes.get_yticks()
    axes.xaxis.set_major_locator(FixedLocator(xticks))
    axes.yaxis.set_major_locator(FixedLocator(yticks))
    axes.set_xticklabels(['{:,}'.format(int(x)) for x in xticks])
    axes.set_yticklabels(['{:,.2%}'.format(x) for x in yticks/100])

def format_ticks_dd(axes):
    xticks = axes.get_xticks()
    yticks = axes.get_yticks()
    axes.xaxis.set_major_locator(FixedLocator(xticks))
    axes.yaxis.set_major_locator(FixedLocator(yticks))
    axes.set_xticklabels(['{:,}'.format(int(x)) for x in xticks])
    axes.set_yticklabels(['{:,}'.format(int(x)) for x in yticks])
