# This strategy makes ~1-3% daily, but the chosen stocks have large spreads

setwd("stonksanalysis")
source("prep_data.R", local=T)
source("regression_strategy.R", local=T)

library(tidyquant)

train_start = "2019-02-01"
train_end = "2020-02-01"
test_start = "2020-05-01"
test_end = "2021-05-01"
oos_start = "2021-05-01"
today = Sys.Date()
tomorrow = Sys.Date()+1

chosenstocks = TTR::stockSymbols()$Symbol
chunksize = 500

chosenstockdat = split(chosenstocks, ceiling(seq_along(chosenstocks)/chunksize)) %>%
  lapply(tq_get, from = train_start, to = tomorrow) %>% 
  rbindlist

system.time({chosenstockdat=tq_get(sample(chosenstocks,chunksize), from = train_start, to = tomorrow) %>% data.table}) #takes about a minute per 100 stonks

regression_results = chosenstockdat %>%
  basic_prep(
    rename_from=c("symbol","date","adjusted"),
    rename_to=c("stock","Date","AdjClose")) %>%
  regression_prep(target_range=7) %>%
  regression_strategy(
    train_start = train_start,
    train_end = train_end,
    test_start = test_start,
    test_end = test_end,
    oos_start = oos_start,
    oos_end = tomorrow)


regression_results[error_improvement>.015 & PredDiff>.01 & training_samples>100, 
        .(sum(CloseDiff, na.rm=T),mean(CloseDiff, na.rm=T),.N)]
regression_results[error_improvement>.015 & PredDiff>.01 & training_samples>100, 
        .(sum(CloseDiff, na.rm=T),mean(CloseDiff, na.rm=T),.N),Date][order(Date)]
regression_results[error_improvement>.015 & PredDiff>.01 & training_samples>100, 
        .(sum(CloseDiff, na.rm=T),mean(CloseDiff, na.rm=T),.N),stock][order(stock)]

strategy(stockdatPrepped, chosenstocks,
         train_start = train_start,
         train_end = train_end,
         test_start = test_start,
         test_end = today,
         oos_start = today,
         oos_end = tomorrow)[error_improvement>.015 & PredDiff>.01 & training_samples>100]