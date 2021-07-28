setwd("stonksanalysis")
source("prep_data.R", local=T)
source("polygon.R", local=T)
source("crossover_strategy.R", local=T)

library(tidyquant)
POLYKEY = Sys.getenv('POLYGONKEY')

parameterset = expand.grid(short_range=c(3,14,28), mid_range=c(56), long_range=c(100,200,400),
                           buy_trigger=c(0), sell_trigger=c(.2,.25,.3), 
                           cooloff=c(0,100,365), sell_after=c(365,10000), sell_last_day=c(T)
) %>% data.table
# -.1 for buy and -.15 for sell and 7/98 for 2015 stocks

test_date = seq(as.Date('2005-08-01'), as.Date('2019-08-01'), 365)

proc_date = function(date, ins, key, sample_size = nrow(ins)){
  
  print(date)
  ins = ins[sample(.N, size=sample_size)]
  financials = stocklist_from_polygon(key = key, date = date, financials = T)
  
  target_companies = financials[!is.na(marketCapitalization)][order(marketCapitalization, decreasing = T)]$ticker[1:150]
  
  fulldat = target_companies %>% 
    # tq_get %>% data.table %>%
    parallel::mclapply(stock_history,
                       start_date = date-2*365,
                       end_date = as.Date('2022-01-01'),
                       key = POLYKEY, check_ticker=F,
                       mc.cores = 8) %>% rbindlist %>%
    basic_prep(
      rename_from=c("symbol","date","adjusted"),
      rename_to=c("stock","Date","AdjClose")
    )
  
  outs = ins %>% 
    apply(1, as.list) %>%
    parallel::mclapply(crossoverReturns, dat=fulldat, summary=T, date = date, 
                      end_date=date+365*2, transaction_fee=.0001, mc.cores = 2) %>%
    rbindlist %>%
    cbind(ins)
  outs$Date = date
  outs
}

results = list()
for(date in seq(as.Date('2005-08-01'), 
                as.Date('2019-08-01'), 365)){
  date=as.Date(date)
  system.time({ results[[date]]=proc_date(date, parameterset, POLYKEY) })
}
results = rbindlist(results)
results = rbind(results, results1)

with(results, tapply(median_profit, list(buy_trigger, sell_trigger), mean))
with(results, tapply(DaysHeldPerPurchase, list(buy_trigger, sell_trigger), mean))
lm(median_profit~
     as.factor(short_range)+
     as.factor(long_range)+
     as.factor(buy_trigger)+
     as.factor(sell_trigger)+
     as.factor(cooloff)+
     as.factor(sell_after)+
     as.factor(Date)
     ,
   results)%>%summary
# The best for earnings seems to be "buy at 0 crossover and at least 25 days after loss, sell at +/- .3 or after 550 days.
# The length of the range doesn't matter for the median, but having a shorter long range appears to improve the average
# Expected profit over 2 years of 3% per stock plus 4% if selling at .3 after 550 days
lm(avg_days_held~
     as.factor(short_range)+
     as.factor(long_range)+
     as.factor(buy_trigger)+
     as.factor(sell_trigger)+
     as.factor(cooloff)+
     as.factor(sell_after)+
     as.factor(Date)
   ,
   results)%>%summary
# But waiting until .3 unduly increases days held unless holding a maximum of 365 days. 
# Also, having the short window at 14days results in less days held, as does having a longer cooloff
lm(trades~
     as.factor(short_range)+
     as.factor(long_range)+
     as.factor(buy_trigger)+
     as.factor(sell_trigger)+
     as.factor(cooloff)+
     as.factor(sell_after)+
     as.factor(Date)
   ,
   results)%>%summary
# Having the longer crossover windows also reduces the number of transactions, 
# as does a longer cooloff

# So, overall optimal seems to be buying a 14/200 window at 0 crossover, holding 
# until a .3 change or 365 days pass, and in case of a loss, not buying for 150 days

x = data.table(short_range=3, mid_range=14, long_range=150, buy_trigger=c(0), sell_trigger=c(.2), 
               cooloff=0, sell_after=10000, sell_last_day=F) %>%
  crossoverReturns(dat=fulldat[stock %in% target_companies[1:150]], summary = F, date = as.Date('2015-01-01'), 
                   end_date = as.Date('2019-01-01'), transaction_fee=.0001)

x[BuySell!=0 & sample=='test',
  .(returns = abs(cumprod(BuySell*AdjCloseFilled))[.N],
    days_held = cumsum(BuySell/abs(BuySell)*as.integer(Date))[.N],
    purchases = .N/2)
  ,stock][order(returns)][,.(PctPos=mean(returns>1),
                             AvgStockReturn=median(returns), 
                             AvgStockDaysHeld=mean(days_held), 
                             DaysHeldPerPurchase = sum(days_held)/sum(purchases),
                             TotalPurchases=sum(purchases))]

st = 'AAPL'
x[stock==st] %>% with(plot(Date, AdjCloseFilled, type='l'))
x[stock==st] %>% with(points(Date, mid_range_mean, type='l'))
x[stock==st & Own] %>% with(points(Date, LastBought, type='p', col='blue'))
x[stock==st & BuySell<0] %>% with(abline(v=Date, col='blue'))
x[stock==st & BuySell>0] %>% with(abline(v=Date))
x[stock==st] %>% View()
x[sample=='test',sum(Own>0),Date] %>% plot(type='l')

aggregates = x[sample=='test' & Own, .(prop_losing = mean(AdjCloseFilled<LastBought), 
                                       total = .N ),Date][order(Date), .(Date, prop_losing, total, prop_losing_roll = frollmean(fillna(prop_losing, .5), 365, fill=NA, algo="exact", align="right", na.rm=T))] 
with(aggregates, plot(Date, prop_losing, type='l'))
with(aggregates, points(Date, prop_losing_roll, type='l', col='red'))
abline(h=.5)

x[Date=='2005-10-01' & Own>0,stock]

SPY = data.table(tq_get("SVSPX"))
lapply(test_date, function(test_start){
  data.frame(date = test_start,
             market = pct_diff(SPY[date %in% (test_start+(362:367)), mean(adjusted,na.rm=T)],
                               SPY[date %in% (test_start+(-2:2)), mean(adjusted,na.rm=T)],
                               of=SPY[date %in% (test_start+(-2:2)), mean(adjusted,na.rm=T)])
  )}) %>% rbindlist

