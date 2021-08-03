setwd("stonksanalysis")
source("prep_data.R", local=T)
source("polygon.R", local=T)
source("crossover_strategy.R", local=T)

library(tidyquant)
POLYKEY = Sys.getenv('POLYGONKEY')
cores = 24
poly_cores=min(cores*8,48)

choose_tickers = function(date, key, top_n){
  financials = stocklist_from_polygon(key = key, date = date, financials = T, cores = poly_cores)
  financials[!is.na(marketCapitalization)][order(marketCapitalization, decreasing = T)]$ticker[1:top_n]
}

date_returns = function(date, params, fulldat, stocklist){
  
  print(paste("Gettin returns for", date , now(tzone = 'America/New_York')))
  
  date_dat = fulldat[Date>date-365*2.1 & Date<date+365*2.1 & (stock %in% stocklist[[as.character(date)]])]
  
  outs = params %>% 
    apply(1, as.list) %>%
    parallel::mclapply(crossoverReturns, dat=date_dat, summary=T, date = date, 
                       end_date=date+365*2, start_date=date-365*2, transaction_fee=.0001, mc.cores = cores) %>%
    rbindlist %>%
    cbind(params)
  outs$Date = date
  outs
}

backtest = function(dates, parameters, key){
  
  print(paste("Starting. " , now(tzone = 'America/New_York')))
  target_companies = lapply(dates, choose_tickers, key=key, top_n=150)
  names(target_companies)=dates
  
  print(paste("Got the company names for each year " , now(tzone = 'America/New_York')))
  fulldat = target_companies %>% 
    unlist %>% unique %>%
    # tq_get %>% data.table %>%
    parallel::mclapply(stock_history,
                       start_date = min(dates)-2.1*365,
                       end_date = max(dates)+2.1*365,
                       key = key, check_ticker=F,
                       mc.cores = poly_cores) %>% rbindlist %>%
    basic_prep(
      rename_from=c("symbol","date","adjusted"),
      rename_to=c("stock","Date","AdjClose")
    )
  print(paste("Got the data for all the companies. " , now(tzone = 'America/New_York')))
  
  results=lapply(dates, date_returns, params = parameters, fulldat = fulldat, stocklist=target_companies)
  results = rbindlist(results)
  results
}


parameterset = expand.grid(short_range=c(98), mid_range=c(56), long_range=c(300,500),
                           buy_trigger=c(-.1,-.15), cooloff=c(50,90), buy_trigger_days = c(50,75,25),
                           sell_hi=c(.2), sell_lo=c(.15), sell_atr = c(12),
                           sell_days=c(100), sell_last=c(T)
)

results = backtest( seq(as.Date('2005-08-01'), as.Date('2019-08-01'), 365), parameterset, POLYKEY)




results=lapply(dates, date_returns, params = parameterset, fulldat = fulldat, stocklist=target_companies) %>% rbindlist
results_agg = results[,.(avg_profit = mean(avg_profit), avg_profit_sd = sd(avg_profit),
                         median_profit = mean(median_profit), median_profit_sd = sd(median_profit),
                         avg_days_held = mean(avg_days_held+10*trades/stocks), stocks = mean(stocks), 
                         DaysHeldPerPurchase = mean(DaysHeldPerPurchase), trades = mean(trades)),
                      names(parameterset)]
results_agg[order(avg_profit/avg_days_held, decreasing=T)] # in terms of profit per day, long ranges with low sell condition are best



#Examine a single date
date = dates[5]
x = data.table(short_range=150, mid_range=14, long_range=500, 
               buy_trigger=-.1, buy_trigger_days=0,
               sell_hi=c(.15),sell_lo=c(.35),  
               cooloff=0, sell_days=100, sell_last=T, sell_atr=16) %>%
  crossoverReturns(dat=fulldat[stock %in% target_companies[[as.character(date)]]], summary = F, date = date, 
                   end_date = date+365*2, start_date=date-2*365, transaction_fee=.0001)

x[BuySell!=0 & sample=='test',
  .(returns = abs(cumprod(BuySell*AdjCloseFilled))[.N],
    days_held = cumsum(BuySell/abs(BuySell)*as.integer(Date))[.N],
    purchases = .N/2)
  ,stock][order(returns)][,.(PctPos=mean(returns>1),
                             AvgStockReturn=median(returns), 
                             AvgStockDaysHeld=mean(days_held), 
                             DaysHeldPerPurchase = sum(days_held)/sum(purchases),
                             TotalPurchases=sum(purchases))]

st = 'TGT'
x[stock==st] %>% with(plot(Date, AdjCloseFilled, type='l'))
x[stock==st] %>% with(points(Date, mid_range_mean, type='l'))
x[stock==st & Own] %>% with(points(Date, LastBought, type='p', col='blue'))
x[stock==st & BuySell<0] %>% with(abline(v=Date, col='blue'))
x[stock==st & BuySell>0] %>% with(abline(v=Date))
x[stock==st] %>% View()
x[stock==st & BuySell!=0]

aggregates = x[sample=='test' & Own, .(prop_losing = mean(AdjCloseFilled<LastBought),
                                       owned = sum(Own>0),
                                       total = .N ),Date][order(Date)]
aggregates[,prop_losing_roll := frollmean(fillna(prop_losing, .5), 365, fill=NA, algo="exact", align="right", na.rm=T)] 
aggregates[,owned:=owned/max(owned)]
with(aggregates, plot(Date, prop_losing, type='l'))
with(aggregates, points(Date, owned, type='l', col='gray'))
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