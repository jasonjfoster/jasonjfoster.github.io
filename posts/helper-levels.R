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

status_r <- exists("factors_r", inherits = TRUE) && length(factors_r) > 0
status_d <- exists("factors_d", inherits = TRUE) && length(factors_d) > 0

if (status_r && status_d) {
  factors <- unique(c(factors_r, factors_d))
} else if (status_r) {
  factors <- unique(factors_r)
} else if (status_d) {
  factors <- unique(factors_d)
} else {
  factors <- character(0)
}

if (length(factors) > 0) {
  
  quantmod::getSymbols(factors, src = "FRED")
  
  levels_xts <- do.call(merge, c(
    lapply(factors, function(i) {
      z <- get(i, inherits = TRUE)
      colnames(z) <- i
      z
    }),
    list(all = TRUE)
  ))
  
  # if (!exists("returns_xts", inherits = TRUE)) {
    
    returns_xts <- do.call(merge, c(
      lapply(factors, function(i) {
        if (status_r && (i %in% factors_r)) {
          diff(log(levels_xts[ , i]))
        } else if (status_d && (i %in% factors_d)) {
          -diff(levels_xts[ , i]) / 100
        } else {
          NULL
        }
      }),
      list(all = TRUE)
    ))
    
    if (!is.null(returns_xts) && (ncol(returns_xts) > 0)) {
      colnames(returns_xts) <- factors
    }
    
  # }
  
  # if (exists("returns_xts", inherits = TRUE) && !is.null(returns_xts) && (ncol(returns_xts) > 0)) {
    overlap_xts <- roll::roll_mean(returns_xts, scale[["overlap"]],
                                   min_obs = 1, na_restore = TRUE)
  # }
  
}