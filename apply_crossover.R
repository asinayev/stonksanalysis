setwd("stonksanalysis")
source("prep_data.R", local=T)
source("crossover_strategy.R", local=T)

library(tidyquant)

today = Sys.Date()
tomorrow = Sys.Date()+1
chunksize = 500

#Pull symbols and benchmark data
chosenstocks = TTR::stockSymbols()$Symbol
SPY = data.table(tq_get("SVSPX", from = train_start, to = tomorrow))

#Pull data
chosenstockdat = split(chosenstocks, ceiling(seq_along(chosenstocks)/chunksize)) %>%
  lapply(tq_get, from = train_start, to = tomorrow) %>% 
  rbindlist
system.time({chosenstockdat=tq_get(sample(chosenstocks,2000), from = train_start, to = tomorrow) %>% data.table}) #takes about a minute per 100 stonks

#Calculate returns for a range of parameters and dates
returns=function(pars=list('short_range'=3, 'mid_range'=28, 'buy_trigger'=.05, 'sell_trigger'=-.05, 'deathcross'=F, 'profit'=-10, 'date'=as.Date("2017-06-01"))){
  pars=as.list(pars)
  chosenstockdat %>% 
    crossover_prep(pars$short_range,pars$mid_range) %>%
    crossover_strategy(train_start = pars$date-365,
                       train_end = pars$date, pars$date, pars$date+365, 
                       buy_trigger = pars$buy_trigger, sell_trigger = pars$sell_trigger,
                       deathcross=pars$deathcross) %>%
    calcReturns(transaction_fee=.01, profit_cutoff=pars$profit, volume_cutoff=100000, summary=T)
}


chosenstockdat = chosenstockdat %>% 
  basic_prep(
    rename_from=c("symbol","date","adjusted"),
    rename_to=c("stock","Date","AdjClose"),
    end_date = as.Date('2021-06-05'),
    start_date = as.Date('2016-05-29')
  )

inputs = expand.grid(short_range=c(5,7), mid_range=c(35,56), 
                     buy_trigger=c(.05,.1,.15), sell_trigger=c(-.05,-.1,-.15), 
                     deathcross=c(F), profit=c(-.2,0,.2),
                     date= as.Date(c("2017-06-01", "2018-01-01", "2018-06-01", "2019-01-01", "2019-06-01", "2020-01-01", "2020-06-01")) )
system.time({results = lapply(1:nrow(inputs), function(x) returns(inputs[x,]) )})

x = results %>% rbindlist %>% cbind(inputs)

#examine effectiveness
x[stocks>3][order(date, absolute_profit, decreasing = T),.SD[1:5],date]
lm(absolute_profit~short_range+mid_range+buy_trigger+sell_trigger+deathcross+profit,x)%>%summary
# good strategy x[short_range==7 & mid_range==35 & buy_trigger==.1 & sell_trigger==-.1 & profit==0]


#S&P performance in the same interval
lapply(unique(inputs$date), function(test_start){
  data.frame(date = test_start,
    market = pct_diff(SPY[date %in% (test_start+(362:367)), mean(adjusted,na.rm=T)],
             SPY[date %in% (test_start+(-2:2)), mean(adjusted,na.rm=T)],
             of=SPY[date %in% (test_start+(-2:2)), mean(adjusted,na.rm=T)])
  )
})%>%rbindlist

#Manually examine
date = as.Date('2020-06-01')
x = chosenstockdat %>% 
  crossover_prep(7,35) %>%
  crossover_strategy(train_start = date-365,
                     train_end = date, date, date+365, 
                     buy_trigger = .1, sell_trigger = -.1,
                     deathcross=F) %>%
  calcReturns(transaction_fee=.01, profit_cutoff=0, volume_cutoff=100000, summary=F)