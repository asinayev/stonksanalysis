setwd("stonksanalysis")
source("prep_data.R", local=T)
source("polygon.R", local=T)
source("crossover_strategy.R", local=T)

library(tidyquant)
POLYKEY = Sys.getenv('POLYGONKEY')
cores = 4
poly_cores=min(cores*8,48)

choose_tickers = function(date, key, top_n){
  financials = stocklist_from_polygon(key = key, date = date, financials = T, cores = poly_cores)
  financials = financials[!is.na(marketCapitalization)]
  financials[order(marketCapitalization, decreasing = T), 
             .(ticker, ticker_valid_start=date, ticker_valid_end=date+365)][1:top_n]
}

filter_range(fulldat, company_dates){
  fulldat[,stockdate:=Date]
  validrange = fulldat[company_dates, 
                     .(stock, Date=stockdate, valid=TRUE),
                     on=.(stock==ticker, Date>=ticker_valid_start, Date<ticker_valid_end)]
  fulldat = merge(fulldat, validrange, all.x=T, on=c('stock','Date')) 
  fulldat[,valid:=!is.na(valid)]
  fulldat=fulldat[,minValid:=min(ifelse(valid,Date,NA),na.rm=T)-365*2,stock]
  fulldat=fulldat[,maxValid:=max(ifelse(valid,Date,NA),na.rm=T),stock]
  fulldat[Date>=minValid & Date<=maxValid,
          .(stock,Date,AdjClose,high,low,volume,AdjCloseFilled,hiFilled,loFilled,atr, valid)]
}

backtest_dat = function(dates, key){
  
  print(paste("Starting. " , now(tzone = 'America/New_York')))
  target_companies = lapply(dates, choose_tickers, key=key, top_n=150) %>% rbindlist
  
  print(paste("Got the company names for each year " , now(tzone = 'America/New_York')))
  fulldat = target_companies$ticker %>% 
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
    ) %>%
    filter_range(target_companies)
  print(paste("Got the data for all the companies. " , now(tzone = 'America/New_York')))
  fulldat
}


parameterset = expand.grid(short_range=c(7), mid_range=c(56), long_range=c(500),
                           buy_trigger=c(-.15,-.1), cooloff=c(60), buy_trigger_days = c(20),
                           sell_hi=c(.25), sell_lo=c(.225), sell_atr = c(10),
                           sell_days=c(100,120), sell_last=c(T)
)

fulldat = backtest_dat(seq(as.Date('2005-08-01'), as.Date('2019-08-01'), 365),
                       POLYKEY)

results = parameterset %>% 
  apply(1, as.list) %>%
  parallel::mclapply(crossoverReturns, dat=fulldat, summary=T, 
                     transaction_fee=.0001, mc.cores = cores) %>%
  rbindlist %>%
  cbind(parameterset)

results[order(avg_profit/(avg_days_held+10*purchases/stocks), decreasing=T)] # in terms of profit per day, long ranges with low sell condition are best



#Examine a single date
date = dates[1]
x = data.table(short_range=7, mid_range=14, long_range=500, 
               buy_trigger=-.15, buy_trigger_days=17,
               sell_hi=.275,sell_lo=.225,  
               cooloff=0, sell_days=120, sell_last=T, sell_atr=10) %>%
  crossoverReturns(dat=fulldat[stock %in% target_companies3[[as.character(date)]]], summary = F, date = date, 
                   end_date = date+365*15, start_date=date-2*365, transaction_fee=.0001)

x[BuySell!=0 & sample=='test',
  .(returns = abs(cumprod(BuySell*AdjCloseFilled))[.N],
    days_held = cumsum(BuySell/abs(BuySell)*as.integer(Date))[.N],
    purchases = .N/2)
  ,stock][order(returns)][,.(PctPos=mean(returns>1),
                             AvgStockReturn=median(returns), 
                             AvgStockDaysHeld=mean(days_held), 
                             DaysHeldPerPurchase = sum(days_held)/sum(purchases),
                             TotalPurchases=sum(purchases))]

st = 'JCP'
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