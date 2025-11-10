# !pip install git+https://github.com/jasonjfoster/hist.git@main#subdirectory=python
import pandas as pd
import numpy as np
import yfhist as yfh

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
#   
#   # weights = (0.9 ** np.arange(width - 1, -1, -1)).reshape((width, 1))
#   weights = np.ones((width, 1))

factors = list(get_var("factors")) if exists("factors") else []
tickers = list(get_var("tickers")) if exists("tickers") else []

status_f = len(factors) > 0
status_t = len(tickers) > 0

if (status_t):

  prices_ls = yfh.get_data(tickers)
  prices_df = yfh.get_col(prices_ls, "adjclose").set_index("index")
  tickers = prices_df.columns
  
  if (status_f):
    if (exists("returns_df") and isinstance(get_var("returns_df"), pd.DataFrame) and \
      (get_var("returns_df").shape[1] > 0)):
      
      returns_df = pd.concat([get_var("returns_df"), np.log(prices_df).diff()], axis = 1)
      returns_df.columns = list(factors) + list(tickers)
      
    else:
      returns_df = np.log(prices_df).diff()
    
    if (exists("overlap_df") and isinstance(get_var("overlap_df"), pd.DataFrame) and \
      (get_var("overlap_df").shape[1] > 0)):
        
      overlap_df = pd.concat([get_var("overlap_df"), returns_df[tickers].rolling(scale["overlap"], min_periods = 1).mean()], axis = 1)
      
    else:
      overlap_df = returns_df[tickers].rolling(scale["overlap"], min_periods = 1).mean()
    
  else:
    
    returns_df = np.log(prices_df).diff()
    overlap_df = returns_df[tickers].rolling(scale["overlap"], min_periods = 1).mean()

  if status_f and exists("overlap_df") and \
    (get_var("overlap_df").shape[1] > 0):

    overlap_df = overlap_df.dropna()
    overlap_x_df = overlap_df[factors][-width:] # same dimension as `weights`
    overlap_y_df = overlap_df[tickers][-width:]
