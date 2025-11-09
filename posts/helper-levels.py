import pandas as pd
import numpy as np
import pandas_datareader as pdr # dev version
# from pandas_datareader import data as pdr

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

  # https://pandas-datareader.readthedocs.io/en/latest/remote_data.html
  levels_df = pdr.get_data_fred(factors, start = "1900-01-01")
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
