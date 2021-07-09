source("prep_data.R", local=T)
source("trade_strategy.R", local=T)

install.packages("readxl")
library(tidyquant)
library(TTR)

train_start = "2019-02-01"
train_end = "2020-02-01"
test_start = "2020-07-15"
twodaysago = "2021-07-07"
today = "2021-07-09"

system.time({chosenstocks = TTR::stockSymbols()$Symbol})
system.time({chosenstockdat = tq_get(chosenstocks, from = train_start, to = today) %>% data.table})
stockdatPrepped = prep_data(chosenstockdat, 
                            train_start = train_start,
                            train_end = train_end,
                            test_start = test_start,
                            test_end = today,
                            oos_start = twodaysago,
                            oos_end = today,
                            rename_from=c("symbol","date","adjusted"),rename_to=c("stock","Date","AdjClose")
)
results = strategy(params=c("minPerformance"=.02, "minAlpha"=-100), stockdatPrepped, chosenstocks)
unique(results$stock)