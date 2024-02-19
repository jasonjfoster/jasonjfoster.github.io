import pandas as pd
import numpy as np
import statsmodels.api as sm
import pandas_datareader as pdr # dev version
from scipy.stats import norm, chi2

factors = factors_r + factors_d
width = 252
scale = {"periods": 252, "overlap": 5}

# https://pandas-datareader.readthedocs.io/en/latest/remote_data.html
levels_df = pdr.get_data_fred(factors, start = "1900-01-01")
levels_df.sort_index(axis = 0, inplace = True)

returns_df = levels_df.apply(lambda x: np.log(x).diff() if x.name in factors_r else -x.diff() / 100)
overlap_df = returns_df.rolling(scale["overlap"], min_periods = 1).mean()
returns_df = pd.concat([returns_df, overlap_df], keys = ["returns", "overlap"], axis = 1)
