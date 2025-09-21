get_col <- function(data_ls, col) {
  
  symbols <- names(data_ls)
  
  series_ls <- lapply(symbols, function(symbol) {
    
    df <- data_ls[[symbol]]
    df <- df[ , c("index", col)]
    
    x <- xts::xts(df[[col]], order.by = df[["index"]])
    colnames(x) <- symbol
    
    x
    
  })
  
  result_xts <- do.call(merge, c(series_ls, list(all = TRUE)))
  
  return(result_xts)
  
}

data_ls <- yfhist::get_data(tickers)
prices_xts <- get_col(data_ls, "adjclose")
tickers <- colnames(prices_xts)

returns_xts <- merge(returns_xts, diff(log(prices_xts)))
overlap_xts <- merge(overlap_xts, roll_mean(returns_xts[ , tickers], scale[["overlap"]], min_obs = 1))

# weights <- 0.9 ^ ((width - 1):0)
weights <- rep(1, width)

overlap_xts <- na.omit(overlap_xts)
overlap_x_xts <- tail(overlap_xts[ , factors], width) # same dimension as `weights`
overlap_y_xts <- tail(overlap_xts[ , tickers], width)