library(ggplot2)

quantile_cut <- function(x) {
  cut(
    -x, breaks = quantile(-x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
    labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE
  )
}

factors <- c("SP500", "DTB3")
result_ls <- readRDS("./posts/crowds-r/result_ls.rds")
index_ls <- readRDS("./posts/crowds-r/index_ls.rds")

n_rows <- length(result_ls)
df_ls <- list()

for (i in 1:n_rows) {
  
  df <- data.frame(result_ls[[i]])
  colnames(df) <- c(factors, "performance")
  
  df[["date"]] <- as.Date(index_ls[[i]])
  df[["ticker"]] <- rownames(result_ls[[i]])
  df[["quantile"]] <- quantile_cut(df[["performance"]])
  
  rownames(df) <- NULL
  
  df_ls <- append(df_ls, list(df))
  
}

result_df <- do.call(rbind, df_ls)

result_df <- result_df[ , c("date", "ticker", "performance", "quantile", factors)]

tail(result_df)

quantile_weight <- aggregate(
  cbind(weight = get(factors[1]), performance) ~ date + quantile, result_df,
  mean, na.rm = TRUE
)

tail(quantile_weight)

ggplot() +
  geom_line(data = subset(quantile_weight, quantile %in% c("Q1", "Q4")),
            aes(x = date, y = weight, color = quantile))

ggplot() +
  geom_line(data = subset(quantile_weight, quantile %in% c("Q1", "Q4")),
            aes(x = date, y = performance, color = quantile))