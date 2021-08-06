
setwd("stonksanalysis")
source("prep_data.R", local=T)
source("polygon.R", local=T)
source("crossover_strategy.R", local=T)

library(tidyquant)
POLYKEY = Sys.getenv('POLYGONKEY')
cores = 4
poly_cores=min(cores*8,48)

get_financials = function(date, key){
  print(paste('getting date', date))
  financials = stocklist_from_polygon(key = key, date = date, financials = T, cores = poly_cores)
  financials[!is.na(marketCapitalization),
             .(ticker, ticker_valid_start=date, ticker_valid_end=date+365, marketCapitalization)]
}

filter_range = function(fulldat, company_dates){
  fulldat[,stockdate:=Date]
  validrange = fulldat[company_dates, 
                       .(stock, Date=stockdate, valid=TRUE),
                       on=.(stock==ticker, Date>=ticker_valid_start, Date<ticker_valid_end)]
  fulldat = merge(fulldat, validrange, all.x=T, on=c('stock','Date')) 
  fulldat[,valid:=!is.na(valid)]
  fulldat=fulldat[,minValid:=min(ifelse(valid,Date,NA),na.rm=T)-365*2,stock]
  fulldat=fulldat[,maxValid:=max(ifelse(valid,Date,NA),na.rm=T)+365,stock]
  fulldat[Date>=minValid & Date<=maxValid,
          .(stock,Date,AdjClose,high,low,volume,AdjCloseFilled,hiFilled,loFilled,atr, valid)]
}

backtest_dat = function(dates, key){
  
  print(paste("Starting. " , now(tzone = 'America/New_York')))
  financials = lapply(dates, get_financials, key=key) %>% rbindlist
  print(paste("Got the company names for each year " , now(tzone = 'America/New_York')))
  financials=financials[order(ticker_valid_start, marketCapitalization, decreasing=T)]
  financials[,cap_order := order(marketCapitalization, decreasing = T),ticker_valid_start]
  target_companies=financials[cap_order<250]
  
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

fulldat = backtest_dat(seq(as.Date('2005-08-01'), as.Date('2019-08-01'), 365),
                       POLYKEY)
gc()
parameterset = expand.grid(short_range=c(3,7,49,100,150), long_range=c(250,350,500,650),
                           buy_trigger=c(0,-.1,-.15), cooloff=c(100,400), buy_trigger_days = c(14,21,35,300),
                           sell_hi=c(.3,.15), sell_lo=c(.15,.3), sell_atr = c(7,10,15,100),
                           sell_days=c(70,120,365,365*2), sell_last=c(T)
)


results = parameterset %>% 
  apply(1, as.list) %>%
  parallel::mclapply(crossoverReturns, dat=fulldat, summary_only=T, 
                     transaction_fee=.0001, mc.cores = cores) %>%
  rbindlist %>%
  cbind(parameterset)

results[order(avg_profit/(days_held_per_purchase+30), decreasing=T)]



#Examine a single date
x = data.table(short_range=7, long_range=650, 
               buy_trigger=-.175, buy_trigger_days=17,
               sell_hi=.275,sell_lo=.25,  
               cooloff=400, sell_days=120, sell_last=T, sell_atr=10) %>%
  crossoverReturns(dat=fulldat, summary = F, transaction_fee=.0001)

x[order(sample(nrow(x)))][BuySell>0, .(stock,Date,BuySell*AdjCloseFilled)]
x[BuySell>0,.(PctPos=mean(BuySell*AdjCloseFilled>1),
              AvgStockReturn=median(BuySell*AdjCloseFilled), 
              TotalPurchases=.N)]

st = 'MFG'
x[stock==st] %>% with(plot(Date, AdjCloseFilled, type='l'))
x[stock==st] %>% with(points(Date, mid_range_mean, type='l'))
x[stock==st & Own] %>% with(points(Date, LastBought, type='p', col='blue'))
x[stock==st & BuySell<0] %>% with(abline(v=Date, col='blue'))
x[stock==st & BuySell>0] %>% with(abline(v=Date))
x[stock==st & BuySell!=0]
x[stock==st] %>% View()

x[BuySell>0,.(avg_profit = mean(BuySell*AdjCloseFilled), sales=.N),year(Date)][order(year)]

x[Date=='2005-10-01' & Own>0,stock]

SPY = data.table(tq_get("SVSPX"))
lapply(test_date, function(test_start){
  data.frame(date = test_start,
             market = pct_diff(SPY[date %in% (test_start+(362:367)), mean(adjusted,na.rm=T)],
                               SPY[date %in% (test_start+(-2:2)), mean(adjusted,na.rm=T)],
                               of=SPY[date %in% (test_start+(-2:2)), mean(adjusted,na.rm=T)])
  )}) %>% rbindlist
