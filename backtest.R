setwd("stonksanalysis")
source("prep_data.R", local=T)
source("polygon.R", local=T)
source("crossover_strategy.R", local=T)
source("store_data.R",local=T)

library(tidyquant)
POLYKEY = Sys.getenv('POLYGONKEY')
cores = 2
poly_cores=min(cores*4)

get_financials = function(date, key){
  print(paste('getting date', date))
  financials = stocklist_from_polygon(key = key, date = date, financials = T, cores = poly_cores)
  financials[!is.na(marketCapitalization),
             .(ticker, ticker_valid_start=date, ticker_valid_end=date+365, marketCapitalization)]
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

# fulldat = backtest_dat(seq(as.Date('2005-08-01'), as.Date('2019-08-01'), 365),
#                        POLYKEY)
# store_dt(fulldat)
fulldat = get_dt(name = 'fulldat')

gc()
parameterset = expand.grid(short_range=c(50), long_range=c(250), min_dip=c(0),
                           buy_trigger=c(-.15), buy_trigger_days_max = c(50), buy_trigger_days_min = c(0),
                           buy_atr_min=c(0.02), buy_rsi_max=c(.5),
                           sell_hi=c(.15), sell_lo=c(.25), sell_atr = c(100),
                           sell_days=c(180), sell_last=c(T)
)


results = parameterset %>% 
  apply(1, as.list) %>%
  parallel::mclapply(crossoverReturns, dat=fulldat, summary_only=T, 
                     transaction_fee=.0001, mc.cores = cores) %>%
  rbindlist %>%
  cbind(parameterset)

results[order(avg_profit/(days_held_per_purchase+30), decreasing=T)]

#Examine a single date
x2 = data.table(short_range=c(50), long_range=c(250), max_dip=c(0),
               buy_trigger=c(-.15), buy_trigger_days_max = c(50), buy_trigger_days_min = c(0),
               buy_atr_min=c(.02), buy_rsi_max=c(.5), sell_rsi_min=c(1.1),
               sell_hi=c(.15), sell_lo=c(.25), sell_atr = c(100),
               sell_days=c(180), sell_last=c(T)) %>%
  crossoverReturns(dat=fulldat, summary = F, transaction_fee=.0001)

x[order(sample(nrow(x)))][BuySell>0, .(stock,Date,BuySell*AdjCloseFilled)][order(V3)]
x[BuySell>0,.(PctPos=mean(BuySell*AdjCloseFilled>1),
              AvgStockReturn=median(BuySell*AdjCloseFilled), 
              TotalPurchases=.N)]

st = 'VZ'
x[stock==st] %>% with(plot(Date, AdjCloseFilled, type='l'))
x[stock==st & Own] %>% with(points(Date, LastBought, type='p', col='blue'))
x[stock==st & BuySell<0] %>% with(abline(v=Date, col='blue'))
x[stock==st & BuySell>0] %>% with(abline(v=Date))
x[stock==st & BuySell!=0]
x[stock==st] %>% View()

x[BuySell>0,.(avg_profit = mean(BuySell*AdjCloseFilled), sales=.N),year(Date)][order(year)] %>% with(plot(x=year, y=avg_profit, cex=sales/10))
abline(h=1)

x[Own>0,length(unique(stock)),Date] %>% plot

SPY = data.table(tq_get("SVSPX"))
lapply(test_date, function(test_start){
  data.frame(date = test_start,
             market = pct_diff(SPY[date %in% (test_start+(362:367)), mean(adjusted,na.rm=T)],
                               SPY[date %in% (test_start+(-2:2)), mean(adjusted,na.rm=T)],
                               of=SPY[date %in% (test_start+(-2:2)), mean(adjusted,na.rm=T)])
  )}) %>% rbindlist
