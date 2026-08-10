import os
import pandas as pd
import numpy as np
from fredapi import Fred

pd.set_option("future.infer_string", False) # reticulate does not convert pyarrow-backed strings (pandas >= 3.0)

fred = Fred(api_key = os.getenv("FRED_API_KEY"))

def exists(name):
  
  result = (name in locals()) or (name in globals())
  
  return result

def get_var(name):
  
  if name in locals():
    result = locals()[name]
  elif name in globals():
    result = globals()[name]
  else:
    raise NameError(name)
  
  return result

if not exists("width"):
  width = 252
  
if not (exists("scale") and isinstance(get_var("scale"), dict)):
  scale = {"periods": 252, "overlap": 5}

# if not exists("weights"):
#   # weights = (0.9 ** np.arange(width - 1, -1, -1)).reshape((width, 1))
#   weights = np.ones((width, 1))

factors_r = list(get_var("factors_r")) if exists("factors_r") else []
factors_d = list(get_var("factors_d")) if exists("factors_d") else []

status_r = len(factors_r) > 0
status_d = len(factors_d) > 0

if status_r and status_d:
  factors = list(dict.fromkeys(factors_r + factors_d))
elif status_r:
  factors = list(dict.fromkeys(factors_r))
elif status_d:
  factors = list(dict.fromkeys(factors_d))
else:
  factors = []

status_f = len(factors) > 0

if status_f:
  
  levels_df = pd.concat([fred.get_series(f, observation_start = "1900-01-01").rename(f) for f in factors], axis = 1, sort = False)
  levels_df.sort_index(axis = 0, inplace = True)

  # extended history: splice cached levels beneath the FRED download, i.e. FRED truncates licensed series (see data-raw)
  if (os.path.exists("../../data-raw/h0a0.csv") and os.path.exists("../../data-raw/spx.csv")):

    h0a0_df = pd.read_csv("../../data-raw/h0a0.csv")
    spx_df = pd.read_csv("../../data-raw/spx.csv")

    cache_df = pd.concat([
      pd.Series(h0a0_df["oas"].values / 100, index = pd.to_datetime(h0a0_df["date"]), name = "BAMLH0A0HYM2"), # convert bps to percent
      pd.Series(spx_df["px_last"].values, index = pd.to_datetime(spx_df["date"]), name = "SP500")
    ], axis = 1, sort = False)

    cols = [i for i in factors if i in cache_df.columns]

    if (cols):

      levels_df = levels_df.combine_first(cache_df[cols])
      levels_df = levels_df[factors]
      levels_df.sort_index(axis = 0, inplace = True)

  # if not exists("returns_df"):
    
  returns_ls = []
  
  for i in factors:
    
    if status_r and (i in factors_r):
      result = np.log(levels_df[i]).diff()
    elif status_d and (i in factors_d):
      result = -levels_df[i].diff() / 100
    else:
      result = None
    
    if result is not None:
      
      result.name = i
      returns_ls.append(result)
  
  if returns_ls:
    returns_df = pd.concat(returns_ls, axis = 1)

  # if (exists("returns_df") and isinstance(returns_df, pd.DataFrame) and (returns_df.shape[1] > 0)):
  overlap_df = returns_df.rolling(scale["overlap"], min_periods = 1).mean()
