import os

# open issue: <https://github.com/pydata/pandas-datareader/issues/965>

tickers = ["BAICX"] # fund inception date is "2011-11-28"
prices_df = pdr.get_data_tiingo(tickers, start = "1900-01-01", api_key = os.getenv("TIINGO_API_KEY"))
prices_df = prices_df.pivot_table(index = "date", columns = "symbol", values = "adjClose") \
    .tz_localize(None)

returns_cols = list(zip(["returns"], tickers))
overlap_cols = list(zip(["overlap"], tickers))
returns_df[returns_cols] = np.log(prices_df).diff()
returns_df[overlap_cols] = returns_df[returns_cols].rolling(scale["overlap"], min_periods = 1).mean()
returns_df.sort_index(axis = 1, inplace = True)

# weights = np.array([0.9 ** i for i in range(width - 1, -1, -1)]).reshape((width, 1))
weights = np.array([1] * width).reshape((width, 1))

overlap_df = returns_df.dropna()["overlap"]
overlap_x_df = returns_df.dropna()["overlap"][factors][-width:] # same dimension as `weights`
overlap_y_df = returns_df.dropna()["overlap"][tickers][-width:]
