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
# year        V1         V2  V3 V4    N            V6
# 1: 2012 1.0098351 0.01092582  14  7   15 5.000000 days
# 2: 2013 1.0192029 0.04030391  38 35   69 5.101449 days
# 3: 2014 1.0155033 0.05510823 110 46  193 4.746114 days
# 4: 2015 1.0379531 0.04830962 371 62  520 3.475000 days
# 5: 2016 0.9981584 0.06110595 136 35  200 7.245000 days
# 6: 2017 1.0067291 0.04448623  46 34   76 5.881579 days
# 7: 2018 1.0191128 0.03654886 518 59 1357 5.061901 days
# 8: 2019 1.0177200 0.02909109  61 34   93 4.720430 days
# 9: 2020 1.0292316 0.08994079 470 48  694 4.619597 days
# 10: 2021 1.0238490 0.03332731 223 51  293 5.569966 days
# 11: 2022 1.0332055 0.06113103 248 25  377 6.209549 days
prices[symbol %in% prices[,.N,symbol][N>delta_window,symbol]
       ,running_low:= frollapply(low, min, n = delta_window ),symbol ]
prices[symbol %in% prices[,.N,symbol][N>5,symbol]
       ,run5_high:= frollapply(high, max, n = 5 ),symbol ] 
prices[symbol %in% prices[,.N,symbol][N>delta_window,symbol]
       ,RSI:= frollmean(pmax(0, close-lag1close) ,n = delta_window, align='right',fill=NA)/
              frollmean(pmax(0, lag1close-close) ,n = delta_window, align='right',fill=NA),symbol ]
prices[symbol %in% prices[,.N,symbol][N>delta_window,symbol]
       ,avg_range:= frollmean(high-low ,n = delta_window, align='right',fill=NA),symbol ]


prices[volume>100000 & close>7 & !grepl('short|bear|inverse', name, ignore.case = T) & lead1sellrally/lead1open<2 & 
         sell_rally_day>5 & (running_low == low | RSI<.7) & (((close-low)/(high-low))<.05 ) & (((high/low) > 1.025) | ((avg_range/close) > .02))
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

window = 100
prices[,lagging_corr_long:=NULL]
prices[symbol %in% prices[days_around>window, unique(symbol)], 
       lagging_corr_long:=
         runCor( day_delta, night_delta, window),
       symbol]

min_corr = .3
# year        V1   N
# 1: 2012 0.9986683   5
# 2: 2013 1.0222907  21
# 3: 2014 1.0192777  27
# 4: 2015 1.0101338  88
# 5: 2016 1.0190706  59
# 6: 2017 1.0321804   9
# 7: 2018 1.0224720  57
# 8: 2019 1.0153659  10
# 9: 2020 1.0121389 340
# 0: 2021 1.0185442  33
# 1: 2022 1.0213601  58
prices[(lead1open/close)<.97 & lagging_corr_long< -min_corr & 
         volume%between%c(10000,100000) & close>7,
       .(mean(lead1close/lead1open,na.rm=T),.N), year(date)][order(year)]

# year        V1   N
# 1: 2012 0.9768759   9
# 2: 2013 0.9656270  51
# 3: 2014 0.9655077  46
# 4: 2015 0.9870471 101
# 5: 2016 0.9762301  70
# 6: 2017 0.9718578  25
# 7: 2018 0.9883178  64
# 8: 2019 0.9700785  12
# 9: 2020 0.9894184 315
# 0: 2021 0.9636769 113
# 1: 2022 0.9696509  85
prices[(lead1open/close)>1.03 & lagging_corr_long< -min_corr & 
         volume%between%c(10000,1000000) & close>7,
       .(mean(lead1close/lead1open,na.rm=T),.N), year(date)][order(year)]
