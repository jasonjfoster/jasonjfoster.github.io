invisible(getSymbols(tickers, src = "tiingo", api.key = Sys.getenv("TIINGO_API_KEY"), adjust = TRUE))
prices_xts <- do.call(merge, c(lapply(tickers, function(i) Cl(get(i))), all = TRUE))
colnames(prices_xts) <- tickers
index(prices_xts) <- as.Date(index(prices_xts))

returns_xts <- merge(returns_xts, diff(log(prices_xts)))
overlap_xts <- merge(overlap_xts, roll_mean(returns_xts[ , tickers], scale[["overlap"]], min_obs = 1))

# weights <- 0.9 ^ ((width - 1):0)
weights <- rep(1, width)

overlap_xts <- na.omit(overlap_xts)
overlap_x_xts <- tail(overlap_xts[ , factors], width) # same dimension as `weights`
overlap_y_xts <- tail(overlap_xts[ , tickers], width)