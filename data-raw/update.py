# update cached levels with the latest FRED observations (run then render)

import os
import pandas as pd
from fredapi import Fred

fred = Fred(api_key = os.getenv("FRED_API_KEY"))

h0a0_df = pd.read_csv("data-raw/h0a0.csv")
h0a0_series = fred.get_series("BAMLH0A0HYM2")
h0a0_new = pd.DataFrame({"date": h0a0_series.index.strftime("%Y-%m-%d"),
                         "oas": (h0a0_series * 100).round(2).values}) # convert percent to bps
h0a0_new = h0a0_new.dropna()
h0a0_df = pd.concat([h0a0_df[h0a0_df["date"] < h0a0_new["date"].min()],
                     h0a0_new,
                     h0a0_df[h0a0_df["date"] > h0a0_new["date"].max()]], ignore_index = True)
h0a0_df.to_csv("data-raw/h0a0.csv", index = False, float_format = "%.10g") # match R, i.e. no trailing zeros

spx_df = pd.read_csv("data-raw/spx.csv")
spx_series = fred.get_series("SP500")
spx_new = pd.DataFrame({"date": spx_series.index.strftime("%Y-%m-%d"),
                        "px_last": spx_series.values})
spx_new = spx_new.dropna()
spx_df = pd.concat([spx_df[spx_df["date"] < spx_new["date"].min()],
                    spx_new,
                    spx_df[spx_df["date"] > spx_new["date"].max()]], ignore_index = True)
spx_df.to_csv("data-raw/spx.csv", index = False, float_format = "%.10g") # match R, i.e. no trailing zeros

# run data-raw/data.R to regenerate the R data sets
