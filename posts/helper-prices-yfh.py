# !pip install git+https://github.com/jasonjfoster/hist.git@main#subdirectory=python
import yfhist as yfh

def get_col(data_ls, col):

  series_ls = []

  for symbol, df in data_ls.items():

    df = df.loc[:, ["index", col]].copy()
    df["index"] = pd.to_datetime(df["index"])

    df = df.set_index("index")[col]
    df.name = symbol

    series_ls.append(df)

  result_df = pd.concat(series_ls, axis = 1)

  return result_df

# def get_data(tickers, scale, width, factors, returns_df = None, overlap_df = None):
# 
#   prices_ls = yfh.get_data(tickers)
#   prices_df = get_col(prices_ls, "adjclose")
#   tickers = prices_df.columns
# 
#   returns_df = pd.concat([returns_df, np.log(prices_df).diff()], axis = 1)
#   overlap_df = pd.concat([overlap_df, returns_df[tickers].rolling(scale["overlap"], min_periods = 1).mean()], axis = 1)
# 
#   # weights = np.array([0.9 ** i for i in range(width - 1, -1, -1)]).reshape((width, 1))
#   weights = np.array([1] * width).reshape((width, 1))
# 
#   overlap_df = overlap_df.dropna()
#   overlap_x_df = overlap_df.dropna()[factors][-width:] # same dimension as `weights`
#   overlap_y_df = overlap_df.dropna()[tickers][-width:]
# 
#   result = {
#     "prices_df": prices_df,
#     "returns_df": returns_df,
#     "overlap_df": overlap_df,
#     "overlap_x_df": overlap_x_df,
#     "overlap_y_df": overlap_y_df,
#     "weights": weights,
#     "tickers": tickers,
#   }
# 
#   return result

prices_ls = yfh.get_data(tickers)
prices_df = get_col(prices_ls, "adjclose")
tickers = prices_df.columns

returns_df = pd.concat([returns_df, np.log(prices_df).diff()], axis = 1)
overlap_df = pd.concat([overlap_df, returns_df[tickers].rolling(scale["overlap"], min_periods = 1).mean()], axis = 1)

# weights = np.array([0.9 ** i for i in range(width - 1, -1, -1)]).reshape((width, 1))
weights = np.array([1] * width).reshape((width, 1))

overlap_df = overlap_df.dropna()
overlap_x_df = overlap_df.dropna()[factors][-width:] # same dimension as `weights`
overlap_y_df = overlap_df.dropna()[tickers][-width:]
