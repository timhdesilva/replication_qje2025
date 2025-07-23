import numpy as np
import statsmodels.api as sm
import pandas as pd

# Imports
from directories import FORPATH

# Actual 2005 tax function
def calculate_PostTaxY(rincome):
    if rincome <= 6000:
        PostTaxY = rincome
    elif rincome <= 21600:
        PostTaxY = rincome - (rincome - 6000) * 0.17
    elif rincome <= 58000:
        PostTaxY = rincome - (21600 - 6000) * 0.17 - (rincome - 21600) * 0.3
    elif rincome <= 70000:
        PostTaxY = rincome - (21600 - 6000) * 0.17 - (58000 - 21600) * 0.3 - (rincome - 58000) * 0.42
    else:
        PostTaxY = rincome - (21600 - 6000) * 0.17 - (58000 - 21600) * 0.3 - (70000 - 58000) * 0.42 - (rincome - 70000) * 0.47
    return PostTaxY
calculate_PostTaxY = np.vectorize(calculate_PostTaxY)

# Fit HSV approximation and output results
incomes = np.arange(1, 500000, 1)
posttax = np.log(calculate_PostTaxY(incomes))
X = sm.add_constant(np.log(incomes))
model = sm.OLS(posttax, X)
results = model.fit()
pd.DataFrame(results.params).to_csv(FORPATH / 'smoothed_tax.txt', header = None, index = None, sep = '\t')
