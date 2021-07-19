setwd("stonksanalysis")
source("prep_data.R", local=T)
source("crossover_strategy.R", local=T)

# GOod for index funds 3 vs 56 range with -.05 buy trigger and -.15 sell trigger

library(tidyquant)

chunksize = 500

#Pull symbols and benchmark data
# allstocks = stocklist_from_polygon(key = POLYKEY,exchange = c('XNYS','XNAS','XNGS'),
#                                    date='2017-01-01')
# market_caps = parallel::mclapply(allstocks$ticker, 
#                        ticker_info_from_polygon, 
#                        key=POLYKEY, 
#                        date='2017-01-01', wait=.1,
#                        field = 'market_cap', mc.cores = 4)
# allstocks = allstocks[sapply(market_caps, function(x)length(x)==1)==T]
# allstocks[, market_cap := unlist(market_caps)]

mc2015 = c('AAPL', 'GOOG','BRK-B','XOM','MSFT','WFC','JNJ', 'WMT', "GE", 'FB', 'JPM', 
           'PG' ,'PFE', 'VZ', 'CVX', 'ORCL', 'DIS', 'COKE', 'AMZN', 'T')

fulldat = data.table(tq_get(mc2015)) %>% 
  basic_prep(
    rename_from=c("symbol","date","adjusted"),
    rename_to=c("stock","Date","AdjClose")
  )


parameterset = expand.grid(short_range=c(3,7,14), mid_range=c(28,56,98), 
                     buy_trigger=c(-.1,-.05,0,.05,.1,.15), sell_trigger=c(.1,.05,0,-.05,-.1,-.15), 
                     deathcross=c(F), profit=c(-10)
) %>% data.table

test_date = seq(as.Date('2015-01-01'), as.Date('2020-01-01'), 365)

proc_date = function(date, ins){
  outs = ins %>%
    apply(1, crossoverReturns, dat=fulldat, summary=T, date = date, transaction_fee=.0001) %>%
    rbindlist %>%
    cbind(ins)
  outs$Date = date
  outs
}

system.time({results = parallel::mclapply(dates, ins=parameterset, proc_date, mc.cores = 4)})

outs[order(absolute_profit,decreasing = T)][1:10,]  
outs[order(absolute_profit,decreasing = T)][1,]  %>% totalReturns(fulldat[stock=='F'], summary = F)
lm(absolute_profit~short_range+mid_range+buy_trigger+sell_trigger+deathcross+profit,x)%>%summary

x = parameterset[short_range==5 & mid_range==98 & buy_trigger==0 & 
                           sell_trigger==0 & !deathcross] %>%
  crossoverReturns( dat=fulldat, summary = F, date = as.Date('2017-01-01'), transaction_fee=.0001)
x[,.(returns = abs(cumprod((Sell-Buy)*AdjCloseFilled))[.N],
     days_held = cumsum((Sell-Buy)/abs(Sell-Buy)*as.integer(Date))[.N] )
  ,stock][,.(mean(returns),mean(days_held))]



SPY = data.table(tq_get("SVSPX"))
lapply(test_date, function(test_start){
  data.frame(date = test_start,
             market = pct_diff(SPY[date %in% (test_start+(362:367)), mean(adjusted,na.rm=T)],
                               SPY[date %in% (test_start+(-2:2)), mean(adjusted,na.rm=T)],
                               of=SPY[date %in% (test_start+(-2:2)), mean(adjusted,na.rm=T)])
  )})

