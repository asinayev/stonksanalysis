source("prep_data.R", local=T)
source("trade_strategy.R", local=T)

install.packages("readxl")
library(tidyquant)
library(TTR)

train_start = "2019-02-01"
train_end = "2020-02-01"
test_start = "2020-07-15"
today = "2021-07-09"
tomorrow = "2021-07-10"

chosenstocks = c("CIX",   "GBR"   ,"GRF"   ,"GSS"   ,"FAS"   ,"IYF"   ,"JHMF"  ,"RIGS"  ,"TOUR"  ,"ZIONP")
system.time({chosenstockdat = tq_get(chosenstocks, from = train_start, to = today) %>% data.table})
stockdatPrepped = prep_data(chosenstockdat, 
          train_start = train_start,
          train_end = train_end,
          test_start = test_start,
          test_end = today,
          oos_start = today,
          oos_end = tomorrow,
          rename_from=c("symbol","date","adjusted"),rename_to=c("stock","Date","AdjClose"),
          future=T
          )

results = strategy(params=c("minPerformance"=.02, "minAlpha"=.002), stockdatPrepped, chosenstocks)
results