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

stocklist = stocklist_from_polygon(key = POLYKEY, date = Sys.Date()-1, details=T, financials=F, cores=16)

prices_m = 
  stocklist[order(market_cap,decreasing=T),head(ticker,20)] %>%
  unique %>%
  parallel::mclapply(
    stock_day,
    start_date='2022-01-01',
    end_date='2023-01-30',
    key=POLYKEY,
    interval='minute',
    day_buffer = 7,
    mc.cores = 20) %>% 
  rbindlist(fill = T)
colnames(prices_m)=c('symbol','close','high','low','volume','TimeStamp','DateTime','open')

setorder(prices_m, symbol, DateTime)

prices_m[, DateTime := as.POSIXct(TimeStamp/1000, 
                                  origin="1970-01-01", tz = 'EST')]
prices_m[,bar_date:=as_date(DateTime)]
prices_m[,bar_hour:=lubridate::hour(DateTime)]


prices_m[,ema_long:=EMA(close ,n = 200, align='right',fill=NA),symbol ]
prices_m[,ema_short:=EMA(close ,n = 25, align='right',fill=NA),symbol ]
prices_m[,rsi_short:=RSI(close ,n = 10, align='right',fill=NA),symbol ]

rally_m(prices_m,
      sell_rule=function(dat){(dat$ema_long>dat$ema_short)|dat$bar_hour==16},
      varname='sell_crossover')
