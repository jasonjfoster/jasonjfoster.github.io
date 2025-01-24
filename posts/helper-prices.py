import os

# open issue: <https://github.com/pydata/pandas-datareader/issues/965>
prices_df = pdr.get_data_tiingo(tickers, start = "1900-01-01", api_key = os.getenv("TIINGO_API_KEY"))
prices_df = prices_df.pivot_table(index = "date", columns = "symbol", values = "adjClose") \
    .tz_localize(None)
prices_df.sort_index(axis = 0, inplace = True)
tickers = prices_df.columns

returns_df = pd.concat([returns_df, np.log(prices_df).diff()], axis = 1)
overlap_df = pd.concat([overlap_df, returns_df[tickers].rolling(scale["overlap"],min_periods = 1).mean()], axis = 1)

# weights = np.array([0.9 ** i for i in range(width - 1, -1, -1)]).reshape((width, 1))
weights = np.array([1] * width).reshape((width, 1))

overlap_df = overlap_df.dropna()
overlap_x_df = overlap_df.dropna()[factors][-width:] # same dimension as `weights`
overlap_y_df = overlap_df.dropna()[tickers][-width:]
