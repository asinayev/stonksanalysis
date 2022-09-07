require(tidyquant)
require(data.table)
require(rpart)
require(ggplot2)

setwd('~/stonksanalysis')
source("polygon.R", local=T)
POLYKEY = Sys.getenv('POLYGONKEY')

# prices=lapply(Sys.Date()-365*10:1, sampled_data, key=POLYKEY, ticker_type='ETF', details=T) %>%   
#   rbindlist(fill=T) %>%
#   dplyr::rename(symbol=stock, close=AdjClose, date=Date)
# prices=lapply(Sys.Date()-365*18:6, sampled_data, key=POLYKEY, ticker_type='INDEX', details=T) %>%   
#   rbindlist(fill=T) %>%
#   dplyr::rename(symbol=stock, close=AdjClose, date=Date) %>% rbind(prices, fill=T)
# 
# setorder(prices, symbol, date)
# prices = prices[!is.na(volume) & !is.na(close) & !is.na(open)]
# prices[,days_around:=cumsum(!is.na(close)),symbol]
# 
# prices[,c("lag1close", "lag2close", "lead1close", "lead2close"):=shift(close, n = c(1:2,-1:-2), type = "lag"),symbol]
# prices[,c("lag1open",  "lag2open", "lead1open", "lead2open"):=shift(open,  n = c(1:2,-1:-2), type = "lag"),symbol]
# prices[,c("lag1high",  "lag2high", "future_day_high" ):=shift(high,  n = c(1,2,-1), type = "lag"),symbol]
# prices[,c("lag1low",   "lag2low"  ):=shift(low,   n = 1:2, type = "lag"),symbol]
# prices[,c("lag1volume"  ):=shift(volume,   n = 1, type = "lag"),symbol]
# prices[,day_delta:= close/open]
# prices[,day_fall:= low/open]
# prices[,day_rise:= high/open]
# prices[,night_delta:= open/lag1close]
# prices[,c("lag1_day_delta",    "lag2_day_delta" , "future_day_delta"  ):=
#          shift(day_delta,    n = c(1,2,-1), type = "lag"),symbol]
# prices[,c("lag1_night_delta",  "lag2_night_delta" , "future_night_delta" ):=
#          shift(night_delta,  n = c(1,2,-1), type = "lag"),symbol]
# prices[,c("lag1_day_fall",    "lag2_day_fall"   ):=shift(day_fall,    n = 1:2, type = "lag"),symbol]
# prices[,c("lag1_day_rise",    "lag2_day_rise", "future_day_rise"   ):=shift(day_rise,    n = c(1:2,-1), type = "lag"),symbol]
# 
# setDTthreads(threads = 4)
# 
# setorder(prices, symbol, date)
# prices[,sell_rally_increment:=ifelse(lag1close<shift(high,n = 2, type="lag") | is.na(shift(high,n = 2, type="lag")), 0, 1),symbol]
# prices[,sell_rally_increment:=cumsum(sell_rally_increment), symbol]
# prices[,sell_rally:=close[.N], .(sell_rally_increment,symbol)]
# prices[,sell_rally_date:=date[.N], .(sell_rally_increment,symbol)]
# prices[,sell_rally_day:=rowid(sell_rally_increment,symbol)]
# prices[,sell_rally_increment:=NULL]
# 
# setorder(prices, symbol, date)
# prices[,sell_mono_increment:=ifelse(lag1close<(lag1low+.8*(lag1high-lag1low))|is.na(lag1high), 0, 1),symbol]
# prices[,sell_mono_increment:=cumsum(sell_mono_increment), symbol]
# prices[,sell_mono:=close[.N], .(sell_mono_increment,symbol)]
# prices[,sell_mono_date:=date[.N], .(sell_mono_increment,symbol)]
# prices[,sell_mono_day:=rowid(sell_mono_increment,symbol)]
# prices[,sell_mono_increment:=NULL]
# 
# 
# 
# 
# sell_rally_avg = function(price_dat){
#   days=nrow(price_dat)
#   return( mean(ifelse(price_dat[,1]>price_dat[days,5], 
#                       price_dat[days,2], price_dat[,4])/price_dat[,3]))
# }
# 
sell_rally_window=200
delta_window=25
# 
# prices[,sell_rally_avg:=NULL]
# system.time({
#   setorder(prices, symbol, date)
#   prices[symbol %in% prices[,.N,symbol][N>sell_rally_window,symbol]
#          ,
#          sell_rally_avg:= zoo::rollapply(data=.SD[,.(sell_rally_date=as.integer(sell_rally_date),
#                                                      close,
#                                                      open,
#                                                      sell_rally,
#                                                      date=as.integer(date))],
#                                          FUN=sell_rally_avg,
#                                          width=sell_rally_window, align='right',by.column = FALSE,fill=NA
#          ),symbol ]
# }
# )
prices=fread("~/datasets/etf_prices_15y.csv")

# Rally ETFs
#         perf  drawdown days_traded
# 1: 0.02586667    -12.4        1078
#    year average drawdown total trades days_traded avg_days_held stocks_traded
# 1: 2008   0.074      5.9   7.2     98          36 4.193878 days            17
# 2: 2009   0.007     -0.2   1.1    152          82 4.631579 days            11
# 3: 2010   0.011     -0.1   1.3    117          96 3.777778 days             8
# 4: 2011   0.027     -0.2   4.6    173          83 4.358382 days            15
# 5: 2012   0.021      0.9   0.4     21          15 2.190476 days             8
# 6: 2013   0.031      0.7   0.5     17          15 7.117647 days             3
# 7: 2014  -0.015     -0.8  -0.6     40          32 3.800000 days             8
# 8: 2015   0.066     -0.7   5.2     79          35 2.898734 days            18
# 9: 2016   0.012     -3.6   4.0    336         121 5.529762 days            22
# 0: 2017   0.018     -0.7   1.4     79          68 3.455696 days             9
# 1: 2018   0.018     -0.5   0.9     51          49 5.215686 days            10
# 2: 2019   0.034     -0.5   3.4    100          72 4.440000 days             7
# 3: 2020   0.018    -12.4   9.6    544         109 4.922794 days            79
# 4: 2021   0.027      0.3  18.0    666         170 4.211712 days            44
# 5: 2022   0.039     -0.2  14.6    375          95 3.512000 days            45

setorder(prices, symbol, date)
prices[,lead1sellrally:= shift(sell_rally,1,type='lead'),symbol ]
prices[,lead1sellrallydate:= shift(sell_rally_date,1,type='lead'),symbol ]
prices[,lead1sellmono:= shift(sell_mono,1,type='lead'),symbol ]

prices[,delta_avg:=NULL]
prices[symbol %in% prices[,.N,symbol][N>delta_window,symbol],
       delta_avg:= SMA(close/lag1close, n = delta_window ),symbol ]
prices[symbol %in% prices[,.N,symbol][N>delta_window,symbol],
       volume_avg:= SMA(volume, n = delta_window ),symbol ]
prices[symbol %in% prices[,.N,symbol][N>5,symbol],
       delta_mn:= SMA(close/lag1close, n = 5 ),symbol ]
prices[,short:=grepl('short|bear|inverse', name, ignore.case = T)]

prices[volume>75000 & close>7 & !short &
         lead1sellrally/lead1open<2 & 
         close<lag1high & sell_rally_day>2 &
         ((sell_rally_avg-delta_avg)/sell_rally_avg)>.018]%>%
  with(performance(date,lead1sellrally/lead1open-1,lead1sellrallydate-date,symbol))

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


potential_buys = prices[volume>75000 & close>7 & !grepl('short|bear|inverse', name, ignore.case = T) &
                          lead1sellrally/lead1open<2]
potential_buys[!is.na(sell_rally_avg),
               rownum:=order(delta_mdn, decreasing = T),date] 

potential_buys[
         lead1sellrally/lead1open<2 & 
         (((close-low)/(high-low))<.05 ) & 
         ((high/close) > 1.05 |
           ((running_low == low | RSI<.6) & ((avg_range/close) > .05)
           ) 
         )][order(rownum),head(.SD,5),date]%>%
  with(performance(date,lead1sellrally/lead1open-1,lead1sellrallydate-date,symbol))

potential_buys[
  lead1sellrally/lead1open<2 & 
    (((close-low)/(high-low))<.05 ) & 
    ((high/close) > 1.075 |
       ((running_low == low | RSI<.6) & ((avg_range/close) > .05)
       ) 
    )]%>%
  with(performance(date,lead1sellrally/lead1open-1,lead1sellrallydate-date,symbol))



window = 100
prices[,lagging_corr_long:=NULL]
prices[symbol %in% prices[days_around>(window+delta_window), unique(symbol)],
       lagging_corr_long:=
         runCor( day_delta, delta_mn, window),
       symbol]

min_corr = .4
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
prices[,rownum:=order(lagging_corr_long, decreasing = F),date] 

prices[delta_mn<.97 & lagging_corr_long> min_corr  &
         volume>100000 & close>7][order(rownum),.SD[1:5],date]%>%
  with(performance(date,lead1sellrally/lead1open-1,lead1sellrallydate-date,symbol))


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
prices[(lag1close/lag1open)<.97 & lagging_corr_long< -min_corr & 
         volume>100000 & close>7,
       .(mean(lead1close/lead1open,na.rm=T),.N), year(date)][order(year)]
