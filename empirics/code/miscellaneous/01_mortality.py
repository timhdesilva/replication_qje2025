import pandas as pd

# Imports
from directories import FORPATH

# Read in data
males = pd.read_excel('../../data/mortality_tables/males_alltables_qx.xls')
females = pd.read_excel('../../data/mortality_tables/females_alltables_qx.xls')

# Take most recent tables from 2005-07
males = males[['Age', '2005-07']].rename(columns = {'2005-07':'m'})
females = females[['Age', '2005-07']].rename(columns = {'2005-07':'f'})

# Merge and average
df = males.merge(females, on = 'Age')
df['avg'] = (df['m'] + df['f']) / 2

# Output
df['Age'] = df['Age'].apply(lambda x: '%.8f' % x)
df['avg'] = df['avg'].apply(lambda x: '%.8f' % x)
df[['Age', 'avg']].to_csv(FORPATH / 'mortality.txt', header = None, index = None, sep = '\t')
