if (!exists("width", inherits = TRUE)) {
  width <- 252
}

if (!(exists("scale", inherits = TRUE) && is.list(get("scale", inherits = TRUE)))) {
  scale <- list("periods" = 252, "overlap" = 5)
}

# if (!exists("weights", inherits = TRUE)) {
#   
#   # weights <- 0.9 ^ ((width - 1):0)
#   weights <- rep(1, width)
#   
# }

status_f <- exists("factors", inherits = TRUE) && length(factors) > 0
status_t <- exists("tickers", inherits = TRUE) && length(tickers) > 0

if (status_t) {
  tickers <- unique(tickers)
} else {
  tickers <- character(0)
}

if (status_t) {
  
  data_ls <- yfhist::get_data(tickers)
  prices_xts <- xts::as.xts(yfhist::get_col(data_ls, "adjclose"))
  tickers <- colnames(prices_xts)
  
  if (status_f) {
    if (exists("returns_xts", inherits = TRUE) && (ncol(returns_xts) > 0)) {
      
      returns_xts <- do.call(merge, c(
        list(returns_xts, diff(log(prices_xts))),
        list(all = TRUE)
      ))
      colnames(returns_xts) <- c(factors, tickers)
      
    }
    
    if (exists("overlap_xts", inherits = TRUE) && (ncol(overlap_xts) > 0)) {
      
      overlap_xts <- do.call(merge, c(
        list(overlap_xts,
             roll::roll_mean(returns_xts[ , tickers], scale[["overlap"]],
                             min_obs = 1, na_restore = TRUE)),
        list(all = TRUE)
      ))
      colnames(overlap_xts) <- c(factors, tickers)
      
    } else {
      overlap_xts <- roll::roll_mean(returns_xts[ , tickers], scale[["overlap"]],
                                     min_obs = 1, na_restore = TRUE)
    }
    
  } else {
    
    returns_xts <- diff(log(prices_xts))
    overlap_xts <- roll::roll_mean(returns_xts[ , tickers], scale[["overlap"]],
                                   min_obs = 1, na_restore = TRUE)
    
  }
  
  # if (exists("factors", inherits = TRUE) && (length(factors) > 0) &&
  #     exists("overlap_xts", inherits = TRUE) && (ncol(overlap_xts) > 0)) {
  #   
  #   overlap_xts <- na.omit(overlap_xts)
  #   overlap_x_xts <- tail(overlap_xts[ , factors], width) # same dimension as `weights`
  #   overlap_y_xts <- tail(overlap_xts[ , tickers], width)
  #   
  # }
  
}