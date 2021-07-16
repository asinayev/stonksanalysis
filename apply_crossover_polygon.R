# setwd("stonksanalysis")
POLYKEY = Sys.getenv('POLYGONKEY')
source("prep_data.R", local=T)
source("crossover_strategy.R", local=T)

library(tidyquant)

today = as.Date('2019-01-01')

chosenstocks = stocklist_from_polygon(POLYKEY, date=today, exchange = c('XNYS','XNAS','XASE'))

chosenstockdat = chosenstocks$ticker %>% unique %>% sample(1000) %>%
  parallel::mclapply(stock_history,
                     start_date = today-365*2, 
                     end_date = today+365, 
                     key = POLYKEY,
                     mc.cores=24) %>%
  rbindlist %>% 
  basic_prep %>%
  crossover_prep(7,35) %>%
  crossover_strategy(train_start = today-365,
                     train_end = today, today, today+365, 
                     buy_trigger = .1, sell_trigger = -.1,
                     deathcross=F) %>%
  calcReturns(transaction_fee=.01, profit_cutoff=0, volume_cutoff=100000, summary=F)

calcReturnsInPeriod(chosenstockdat, 'test', transaction_fee=.01)[,.(absolute_profit = mean(absolute_profit),
                                                                    stocks=.N,
                                                                    trades = sum(trades) )]
calcReturnsInPeriod(chosenstockdat, 'test', transaction_fee=.01)[order(absolute_profit)]
