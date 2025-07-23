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

# Fringe benefit thresholds
fringeb_tsh = {
    1989:1932,
    1990:1932,
    1991:1932,
    1992:1932,
    1993:1932,
    1994:1835,
    1995:1937,
    1996:1941,
    1997:1941,
    1998:1949,
    1999:1941,
    2000:1941,
    2001:1941,
    2002:1941,
    2003:1941,
    2004:1941,
    2005:1941,
    2006:1941,
    2007:1869,
    2008:1869,
    2009:3738,
    2010:3738,
    2011:3738,
    2012:3738,
    2013:3738,
    2014:3738,
    2015:3773,
    2016:3773,
    2017:3773,
    2018:3773,
    2019:3773
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

# Function for clipping outliers on iqr
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

def mask_outlier(df_in, cols, n_iqr = 5):
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
        df[col_name] = df[col_name].mask(df[col_name] <= fence_low)
        df[col_name] = df[col_name].mask(df[col_name] >= fence_high)
    return df

# Function to convert pandas series to .txt
def series_txt(series, file):
    series = series.apply(lambda x: '%.8f' % x)
    series.to_csv(file, header = None, index = None, sep = '\t')
