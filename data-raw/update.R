# update cached levels with the latest FRED observations (run then render)

h0a0_df <- read.csv("data-raw/h0a0.csv")
h0a0_xts <- quantmod::getSymbols("BAMLH0A0HYM2", src = "FRED", auto.assign = FALSE)
h0a0_new <- data.frame(date = format(zoo::index(h0a0_xts)),
                       oas = round(as.numeric(h0a0_xts) * 100, 2)) # convert percent to bps
h0a0_new <- na.omit(h0a0_new)
h0a0_df <- rbind(h0a0_df[h0a0_df[["date"]] < min(h0a0_new[["date"]]), ],
                 h0a0_new,
                 h0a0_df[h0a0_df[["date"]] > max(h0a0_new[["date"]]), ])
write.csv(h0a0_df, "data-raw/h0a0.csv", row.names = FALSE, quote = FALSE)

spx_df <- read.csv("data-raw/spx.csv")
spx_xts <- quantmod::getSymbols("SP500", src = "FRED", auto.assign = FALSE)
spx_new <- data.frame(date = format(zoo::index(spx_xts)),
                      px_last = as.numeric(spx_xts))
spx_new <- na.omit(spx_new)
spx_df <- rbind(spx_df[spx_df[["date"]] < min(spx_new[["date"]]), ],
                spx_new,
                spx_df[spx_df[["date"]] > max(spx_new[["date"]]), ])
write.csv(spx_df, "data-raw/spx.csv", row.names = FALSE, quote = FALSE)

source("data-raw/data.R")
