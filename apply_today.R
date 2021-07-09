source("stonkanalysis/prep_data.R")
source("stonkanalysis/trade_strategy.R")
library(tidyquant)

tickers = c("AAPL", "NFLX", "AMZN", "K", "O")

getSymbols(tickers,
           from = "2020-06-01",
           to = "2021-08-01")