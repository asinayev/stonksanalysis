setwd("stonksanalysis")
source("prep_data.R", local=T)
source("crossover_strategy.R", local=T)

library(tidyquant)

today = Sys.Date()
tomorrow = Sys.Date()+1
chunksize = 500

#Pull symbols and benchmark data
chosenstocks = TTR::stockSymbols()$Symbol

#Pull data
chosenstockdat = split(chosenstocks, ceiling(seq_along(chosenstocks)/chunksize)) %>%
  lapply(tq_get, from = train_start, to = tomorrow) %>% 
  rbindlist
system.time({chosenstockdat=tq_get(sample(chosenstocks,2000), from = train_start, to = tomorrow) %>% data.table}) #takes about a minute per 100 stonks

#Calculate returns for a range of parameters and dates

chosenstockdat = chosenstockdat %>% 
  basic_prep(
    rename_from=c("symbol","date","adjusted"),
    rename_to=c("stock","Date","AdjClose")
  ) %>% 
  crossover_prep(7,35) %>%
  crossover_strategy(train_start = today-365,
                     train_end = today-200, today-200, today+1, 
                     buy_trigger = .1, sell_trigger = -.1,
                     deathcross=F, sell_last_day = F)
