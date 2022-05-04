require(tidyquant)
require(data.table)
require(rpart)
require(ggplot2)

setwd('~/stonksanalysis')
source("polygon.R", local=T)
POLYKEY = Sys.getenv('POLYGONKEY')

prices=lapply(Sys.Date()-365*10:1, sampled_data, key=POLYKEY, ticker_type='ETF', details=T) %>%   
  rbindlist(fill=T) %>%
  dplyr::rename(symbol=stock, close=AdjClose, date=Date)
prices=lapply(Sys.Date()-365*10:6, sampled_data, key=POLYKEY, ticker_type='INDEX', details=T) %>%   
  rbindlist(fill=T) %>%
  dplyr::rename(symbol=stock, close=AdjClose, date=Date) %>% rbind(prices, fill=T)

setorder(prices, symbol, date)
prices = prices[!is.na(volume) & !is.na(close) & !is.na(open)]
prices[,days_around:=cumsum(!is.na(close)),symbol]

prices[,c("lag1close", "lag2close", "lead1close"):=shift(close, n = c(1:2,-1), type = "lag"),symbol]
prices[,c("lag1open",  "lag2open", "lead1open"):=shift(open,  n = c(1:2,-1), type = "lag"),symbol]
prices[,c("lag1high",  "lag2high", "future_day_high" ):=shift(high,  n = c(1,2,-1), type = "lag"),symbol]
prices[,c("lag1low",   "lag2low"  ):=shift(low,   n = 1:2, type = "lag"),symbol]
prices[,c("lag1volume"  ):=shift(volume,   n = 1, type = "lag"),symbol]
prices[,day_delta:= close/open]
prices[,day_fall:= low/open]
prices[,day_rise:= high/open]
prices[,night_delta:= open/lag1close]
prices[,low_running:= frollapply(close, min, n = 50 ),symbol ]
prices[,c("lag1_day_delta",    "lag2_day_delta" , "future_day_delta"  ):=
         shift(day_delta,    n = c(1,2,-1), type = "lag"),symbol]
prices[,c("lag1_night_delta",  "lag2_night_delta" , "future_night_delta" ):=
         shift(night_delta,  n = c(1,2,-1), type = "lag"),symbol]
prices[,c("lag1_day_fall",    "lag2_day_fall"   ):=shift(day_fall,    n = 1:2, type = "lag"),symbol]
prices[,c("lag1_day_rise",    "lag2_day_rise", "future_day_rise"   ):=shift(day_rise,    n = c(1:2,-1), type = "lag"),symbol]

setDTthreads(threads = 4)

setorder(prices, symbol, date)
prices[,sell_rally_increment:=ifelse(lag1close<shift(high,n = 2, type="lag") | is.na(shift(high,n = 2, type="lag")), 0, 1),symbol]
prices[,sell_rally_increment:=cumsum(sell_rally_increment), symbol]
prices[,sell_rally:=close[.N], .(sell_rally_increment,symbol)]
prices[,sell_rally_date:=date[.N], .(sell_rally_increment,symbol)]
prices[,sell_rally_day:=rowid(sell_rally_increment,symbol)]
prices[,sell_rally_increment:=NULL]


sell_rally_avg = function(price_dat){
  days=nrow(price_dat)
  return( mean(ifelse(price_dat[,1]>price_dat[days,5], 
                      price_dat[days,2], price_dat[,4])/price_dat[,3]))
}

sell_rally_window=200
delta_window=25

prices[,sell_rally_avg:=NULL]
system.time({
  setorder(prices, symbol, date)
  prices[symbol %in% prices[,.N,symbol][N>sell_rally_window,symbol]
         ,
         sell_rally_avg:= zoo::rollapply(data=.SD[,.(sell_rally_date=as.integer(sell_rally_date),
                                                     close,
                                                     open,
                                                     sell_rally,
                                                     date=as.integer(date))],
                                         FUN=sell_rally_avg,
                                         width=sell_rally_window, align='right',by.column = FALSE,fill=NA
         ),symbol ]
}
)

# Rally ETFs
#200 / 25 /.02
#    year       avg         sd stocks days   N          held
# 1: 2013 1.0079214 0.07097431      2    5   5 7.800000 days
# 2: 2014 0.9640603 0.11703296      7   25  33 4.515152 days
# 3: 2015 1.0338281 0.16034269     12   76 113 4.619469 days
# 4: 2016 1.0121188 0.11377559     15  128 316 5.594937 days
# 5: 2017 1.0271979 0.03600538      6   63  80 2.925000 days
# 6: 2018 1.0207102 0.09507518      9   32  35 4.742857 days
# 7: 2019 1.0360372 0.05695500      7   69  87 4.091954 days
# 8: 2020 1.0250528 0.18287416     53   91 409 5.068460 days
# 9: 2021 1.0281825 0.06120994     36  151 533 4.272045 days
# 0: 2022 1.0637641 0.08052805     14   49  98 3.867347 days

setorder(prices, symbol, date)
prices[,lead1sellrally:= shift(sell_rally,1,type='lead'),symbol ]
prices[,lead1sellrallydate:= shift(sell_rally_date,1,type='lead'),symbol ]

prices[,delta_avg:=NULL]
prices[symbol %in% prices[,.N,symbol][N>delta_window,symbol],
       delta_avg:= SMA(close/lag1close, n = delta_window ),symbol ]
prices[volume>100000 & close>5 & !grepl('short|bear|inverse', name, ignore.case = T) &
         date!=sell_rally_date & 
         sell_rally_day>2 &
         lead1sellrally/lead1open<2 & (sell_rally_avg-delta_avg)>.015 
       , .(avg = mean(lead1sellrally/lead1open,na.rm=T),sd = sd(lead1sellrally/lead1open,na.rm=T),stocks = length(unique(symbol)), days = length(unique(date)),.N,held=mean(lead1sellrallydate-date))
       , year(date)][order(year)]


# revert ETFs
#    year        V1         V2  V3 V4   N            V6
# 1: 2012 1.0108262 0.03325050  13 10  14 4.428571 days
# 2: 2013 1.0221889 0.04274297  29 23  40 5.525000 days
# 3: 2014 1.0212583 0.05459161  63 23  79 3.886076 days
# 4: 2015 1.0417507 0.07858805  58 30  87 5.781609 days
# 5: 2016 0.9907140 0.05544958  58 26  68 8.294118 days
# 6: 2017 0.9911365 0.05974580  25 23  35 7.200000 days
# 7: 2018 1.0243715 0.02433506 419 40 706 4.968839 days
# 8: 2019 1.0217739 0.05830259  52 42  79 5.417722 days
# 9: 2020 1.0315627 0.08956182 392 55 487 5.098563 days
# 0: 2021 1.0203334 0.05560547 161 43 193 6.538860 days
# 1: 2022 1.0316547 0.05877418 152 20 176 6.181818 days
prices[symbol %in% prices[,.N,symbol][N>delta_window,symbol]
       ,running_low:= zoo::rollapply(low,min,width=delta_window, align='right',fill=NA),symbol ]

prices[volume>100000 & close>10 & sell_rally_day>5 & !grepl('short|bear|inverse', name, ignore.case = T) &
         running_low==low & ((close-low)/(high-low))<.05 & lead1sellrally/lead1open<2 & ((high/low) > 1.025)
       , .( mean(lead1sellrally/lead1open,na.rm=T),sd(lead1sellrally/lead1open,na.rm=T),length(unique(symbol)), length(unique(date)),.N,mean(sell_rally_date-date))
       , year(date)][order(year)]


# Low Close ETFs
#    year      avg         sd stocks days    N          held
# 1: 2012 1.014462 0.04834311     57   47  139 5.920863 days
# 2: 2013 1.024131 0.04450167     58   51  125 5.120000 days
# 3: 2014 1.003969 0.11601048     72   69  189 5.957672 days
# 4: 2015 1.035853 0.08843009    202  112  450 4.777778 days
# 5: 2016 1.020830 0.09294405     82  120  309 6.634304 days
# 6: 2017 1.006130 0.10920584     64   80  177 6.790960 days
# 7: 2018 1.014148 0.10990815    146  104  585 6.745299 days
# 8: 2019 1.021030 0.08597074     77  144  334 6.167665 days
# 9: 2020 1.029696 0.11938325    586  191 1801 5.085508 days
# 0: 2021 1.020013 0.07929712    187  165  681 6.233480 days
# 1: 2022 1.020582 0.08787149    162   65  623 5.444623 days
prices[volume>100000 & close>5 & lead1sellrally/close<2 & !grepl('short|bear|inverse', name, ignore.case = T) &
         ((close-low)/(high-low)) < .05
         & (high/close) > 1.05
       , .(avg = mean(lead1sellrally/close,na.rm=T),sd = sd(lead1sellrally/close,na.rm=T),stocks = length(unique(symbol)), days = length(unique(date)),.N,held=mean(sell_rally_date-date))
       , year(date)][order(year)]
