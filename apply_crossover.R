setwd("stonksanalysis")
source("prep_data.R", local=T)
source("crossover_strategy.R", local=T)

library(tidyquant)

train_start = as.Date("2019-02-01")
train_end = as.Date("2020-02-01")
test_start = as.Date("2020-05-01")
test_end = as.Date("2021-05-01")
oos_start = "2021-05-01"
today = Sys.Date()
tomorrow = Sys.Date()+1

chosenstocks = TTR::stockSymbols()$Symbol
chunksize = 500

SPY = data.table(tq_get("VOO", from = train_start, to = tomorrow))

chosenstockdat = split(chosenstocks, ceiling(seq_along(chosenstocks)/chunksize)) %>%
  lapply(tq_get, from = train_start, to = tomorrow) %>% 
  rbindlist

system.time({chosenstockdat=tq_get(sample(chosenstocks,chunksize), from = train_start, to = tomorrow) %>% data.table}) #takes about a minute per 100 stonks


test_start= as.Date("2020-04-04")

chosenstockdat %>% 
  basic_prep(
    rename_from=c("symbol","date","adjusted"),
    rename_to=c("stock","Date","AdjClose")
    ) %>% 
  crossover_prep(3,28) %>%
  crossover_strategy(train_start = test_start-365+28,
                     train_end = test_start, test_start,test_start+365,trigger_pct_diff = .1) %>%
  calcReturns(transaction_fee=.01, profit_cutoff=.01, volume_cutoff=100000)

pct_diff(SPY[date %in% (as.Date(test_start)+(362:367)), mean(adjusted,na.rm=T)],
         SPY[date %in% (as.Date(test_start)+(-2:2)), mean(adjusted,na.rm=T)],
         of=SPY[date %in% (as.Date(test_start)+(-2:2)), mean(adjusted,na.rm=T)])
