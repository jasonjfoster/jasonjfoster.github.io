library(quantmod)
library(roll)
library(data.table)
options("getSymbols.warning4.0" = FALSE)

factors <- c(factors_r, factors_d)
width <- 252
scale <- list("periods" = 252, "overlap" = 5)

getSymbols(factors, src = "FRED")
levels_xts <- do.call(merge, c(lapply(factors, function(i) get(i)), all = TRUE))

returns_xts <- do.call(merge, lapply(factors, function(i) {
  if (i %in% factors_r) {
    diff(log((levels_xts[ , i])))
  } else if (i %in% factors_d) {
    -diff(levels_xts[ , i]) / 100
  }    
}))
overlap_xts <- roll_mean(returns_xts, scale[["overlap"]], min_obs = 1, na_restore = TRUE)