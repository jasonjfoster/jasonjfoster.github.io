import pandas as pd
import numpy as np
import statsmodels.api as sm
import pandas_datareader as pdr # dev version
from scipy.stats import norm, chi2

factors_r = ["SP500", "DTWEXAFEGS"] # "SP500" does not contain dividends; note: "DTWEXM" discontinued as of Jan 2020
factors_d = ["DGS10", "BAMLH0A0HYM2"]
factors = factors_r + factors_d
width = 252
scale = {"periods": 252, "overlap": 5}

# https://pandas-datareader.readthedocs.io/en/latest/remote_data.html
levels_df = pdr.get_data_fred(factors, start = "1900-01-01")

returns_df = levels_df.apply(lambda x: np.log(x).diff() if x.name in factors_r else -x.diff() / 100)
overlap_df = returns_df.rolling(scale["overlap"], min_periods = 1).mean()
returns_df = pd.concat([returns_df, overlap_df], keys = ["returns", "overlap"], axis = 1)
