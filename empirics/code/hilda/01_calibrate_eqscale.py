import pandas as pd

# Imports
from directories import DATAPATH, FORPATH
from fxns_globals import series_txt

# Function to convert pandas series to .txt
def series_txt(series, file):
    series = series.apply(lambda x: '%.8f' % x)
    series.to_csv(file, header = None, sep = '\t')

# Minimum and maximum ages for all years
min_age = 22
max_age = 90

# Collect data from HILDA waves
waves = {'b':2002, 'c':2003, 'd':2004, 'e':2005, # all waves
         'f':2006, 'g':2007, 'h':2008, 'i':2009,
         'j':2010, 'k':2011, 'l':2012, 'm':2013,
         'n':2014, 'o':2015, 'p':2016, 'q':2017,
         'r':2018, 's':2019} # wave index p.8 of 1.Readme200.pdf
keeps = ['year', 'adults', 'children', 'age_head'] # variables to keep
dfs = []
for w in waves:
    # Read in merged data at household level
    df = pd.read_stata(DATAPATH / f'Household_{w}200c.dta')
    # Calculate household number of adults and childeren
    df['children'] = (
        pd.to_numeric(df[f'{w}hh0_4'], errors = 'coerce') + 
        pd.to_numeric(df[f'{w}hh5_9'], errors = 'coerce') + 
        pd.to_numeric(df[f'{w}hh10_14'], errors = 'coerce')
    )
    df['adults'] = pd.to_numeric(df[f'{w}hhadult'], errors = 'coerce')
    # Calculate age of head of household
    df['age_head'] = pd.to_numeric(df[f'{w}hgage1'], errors = 'coerce')
    df = df.query('age_head >= @min_age and age_head <= @max_age') # age range
    # Append
    df['year'] = waves[w]
    dfs.append(df[keeps])
    
# Concatenate and winsorize
df = pd.concat(dfs, ignore_index = True)

# Calculate Lusardi-Mitchell 17 equivalent scale
es = df.groupby('age_head')[['adults', 'children']].mean()
es['eqscale'] = (es['adults'] + 0.7 * es['children']) ** 0.75

# Output
series_txt(es['eqscale'], FORPATH / 'HILDA_equivscale.txt')
