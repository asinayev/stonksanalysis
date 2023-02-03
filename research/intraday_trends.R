require(tidyquant)
require(data.table)
require(rpart)
require(ggplot2)

setwd('~/stonksanalysis')
source("polygon.R", local=T)
source("implement/features.R", local=T)
source("research/performance.R", local=T)
POLYKEY = Sys.getenv('POLYGONKEY')

rally_m = function(stock_dat,
                 sell_rule=function(dat){dat$close>=dat$lag1high},
                 varname='sell_rally'){
  setorder(stock_dat, symbol, TimeStamp)
  stock_dat[,sell_rally_increment:=shift(sell_rule(.SD),n=1,type='lag'),symbol]
  stock_dat[,sell_rally_increment:=ifelse(is.na(sell_rally_increment),0,sell_rally_increment)]
  stock_dat[,sell_rally_increment:=cumsum(sell_rally_increment), symbol]
  varnames = paste0(varname,c('','_dt','_bar'))
  stock_dat[,c(varnames):=list(close[.N],DateTime[.N],seq_len(.N)),
            .(sell_rally_increment,symbol)]
  stock_dat[,sell_rally_increment:=NULL]
}

stocklist = stocklist_from_polygon(key = POLYKEY, date = Sys.Date()-365*3, details=T, financials=F, cores=16)

prices = stocklist[order(market_cap,decreasing=T),head(ticker,20)]  %>%
  parallel::mclapply(
    stock_history,
    start_date = Sys.Date()-3*365, end_date = Sys.Date()+2, key = POLYKEY, 
    print=F, check_ticker=F,mc.cores = 20) %>% 
  rbindlist(use.names=TRUE, fill=T)

prices = prices[, .SD[1], by=.(stock, Date)][
  ,.(symbol=stock,date=Date, AdjClose, open, high, low, volume, close=AdjClose)]
lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)

prices_m = 
  stocklist[order(market_cap,decreasing=T),head(ticker,20)] %>%
  unique %>%
  parallel::mclapply(
    stock_day,
    start_date = Sys.Date()-3*365, end_date = Sys.Date()+2, key = POLYKEY, 
    interval='minute',
    day_buffer = 7,
    mc.cores = 20) %>% 
  rbindlist(fill = T)
colnames(prices_m)=c('symbol','close','high','low','volume','TimeStamp','DateTime','open')

prices_m2=data.table(prices_m)
prices_m2[, DateTime := as.POSIXct(TimeStamp/1000, 
                                  origin="1970-01-01", tz = 'EST')]
prices_m2[,date:=as_date(DateTime)]
prices_m2[,bar_hour:=lubridate::hour(DateTime)]
prices_m2[,bar_minute:=lubridate::minute(DateTime)]

prices_m2 = prices_m2[(bar_hour*60+bar_minute) %between% c(9*60+30, 11*60),
          .(morn_low=min(low),morn_high=max(high),morn_open=open[1],morn_close=close[.N]),
          .(symbol,date)] %>% 
  merge(prices_m2[(bar_hour*60+bar_minute) %between% c(11*60, 16*60),
                  .(afternoon_low=min(low),afternoon_high=max(high),afternoon_open=open[1],afternoon_close=close[.N]),
                  .(symbol,date)])

prices_m2[]