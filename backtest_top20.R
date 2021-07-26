setwd("stonksanalysis")
source("prep_data.R", local=T)
source("crossover_strategy.R", local=T)

library(tidyquant)

parameterset = expand.grid(short_range=c(3,7,14), mid_range=c(56), long_range=c(100,150,200),
                     buy_trigger=c(-.1,-.05,0,.05,.1), sell_trigger=c(.1,.2,.3), 
                     cooloff=c(), sell_after=c(180,365,550)
) %>% data.table
# -.1 for buy and -.15 for sell and 7/98 for 2015 stocks

test_date = seq(as.Date('2005-08-01'), as.Date('2019-08-01'), 365)

proc_date = function(date, ins){
  
  financials = stocklist_from_polygon(key = POLYKEY, date = date, financials = T)
  
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
    apply(1, crossoverReturns, dat=fulldat, summary=T, date = date, end_date=date+365*2, transaction_fee=.0001) %>%
    rbindlist %>%
    cbind(ins)
  outs$Date = date
  outs
}

system.time({results = parallel::mclapply(test_date[1:2], ins=parameterset[1:10,], proc_date, mc.cores = 2)})

with(results, tapply(absolute_profit, list(short_range, mid_range), mean))
with(results, tapply(absolute_profit, list(buy_trigger, sell_trigger), mean))
lm(absolute_profit~short_range+mid_range+buy_trigger+sell_trigger,results)%>%summary

x = data.table(short_range=3, mid_range=14, long_range=150, buy_trigger=c(0), sell_trigger=c(.2), 
               cooloff=100, sell_after=365) %>%
  crossoverReturns(dat=fulldat[stock %in% target_companies[1:150]], summary = F, date = as.Date('2016-06-04'), 
        end_date = as.Date('2018-06-01'), transaction_fee=.0001)
x[BuySell!=0 & sample=='test',
       .(returns = abs(cumprod(BuySell*AdjCloseFilled))[.N],
         days_held = cumsum(BuySell/abs(BuySell)*as.integer(Date))[.N],
         purchases = .N/2)
       ,stock][order(returns)][,.(PctPos=mean(returns>1),
                                  AvgStockReturn=median(returns), 
                                  AvgStockDaysHeld=mean(days_held), 
                                  DaysHeldPerPurchase = sum(days_held)/sum(purchases),
                                  TotalPurchases=sum(purchases))]

st = 'TI'
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

