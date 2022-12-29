require(tidyquant)
require(data.table)
require(rpart)
require(ggplot2)

setwd('~/stonksanalysis')
source("polygon.R", local=T)
source("implement/features.R", local=T)
source("research/performance.R", local=T)
POLYKEY = Sys.getenv('POLYGONKEY')

# prices=lapply(Sys.Date()-365*10:1, sampled_data, key=POLYKEY, ticker_type='ETF', details=T) %>%   
#   rbindlist(fill=T) %>%
#   dplyr::rename(symbol=stock, close=AdjClose, date=Date)
# prices=lapply(Sys.Date()-365*18:6, sampled_data, key=POLYKEY, ticker_type='INDEX', details=T) %>%   
#   rbindlist(fill=T) %>%
#   dplyr::rename(symbol=stock, close=AdjClose, date=Date) %>% rbind(prices, fill=T)
# 
# setorder(prices, symbol, date)

prices=fread("~/datasets/etf_prices_15y.csv")

prices[,short:=grepl('short|bear|inverse', name, ignore.case = T)]
prices[,lever:=grepl('2x|3x|leverag|ultra', name, ignore.case = T)]

# prices[,RSI2:=NULL ]
# prices[(symbol %in% prices[!is.na(close-lag1close),.N,symbol][N>5,symbol]) & !is.na(close-lag1close),
#        RSI2:=SMA(pmax(0, close-lag1close) ,n = 2, align='right',fill=NA)/
#          SMA(pmax(0, lag1close-close) ,n = 2, align='right',fill=NA),symbol ]
# 
# prices[,PDM:=NULL ]
# prices[(symbol %in% prices[!is.na(close-lag1close),.N,symbol][N>5,symbol]) & !is.na(close-lag1close),
#        PDM:=SMA(pmax(0, high-lag1high) ,n = 3, align='right',fill=NA),
#        symbol ]
# prices[,NDM:=NULL ]
# prices[(symbol %in% prices[!is.na(close-lag1close),.N,symbol][N>5,symbol]) & !is.na(close-lag1close),
#        NDM:=SMA(pmax(0, lag1low-low  ) ,n = 3, align='right',fill=NA),
#        symbol ]
# prices[,ADX:=NULL ]
# prices[(symbol %in% prices[!is.na((PDM-NDM)/(PDM+NDM)),.N,symbol][N>30,symbol]) & !is.na((PDM-NDM)/(PDM+NDM)),
#        ADX:=EMA((PDM-NDM)/(PDM+NDM) ,n = 30, align='right',fill=NA),
#        symbol ]

lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)
prices[,lead1sell_rally:= shift(sell_rally,1,type='lead'),symbol]
prices[,lead1sell_rallydate:= shift(sell_rally_date,1,type='lead'),symbol]

# rally(prices,
#       sell_rule=function(dat){(dat$close-dat$lag1close)>=dat$avg_range*.5},
#       varname='sell_range_up')
# prices[,lead1sell_range_up:= shift(sell_range_up,1,type='lead'),symbol]
# prices[,lead1sell_range_update:= shift(sell_range_up_date,1,type='lead'),symbol]
#                                                                   
# rally(prices,
#       sell_rule=function(dat){(dat$lag1close-dat$close)>=dat$avg_range*.5},
#       varname='sell_range_down')
# prices[,lead1sell_range_down:= shift(sell_range_down,1,type='lead'),symbol]
# prices[,lead1sell_range_downdate:= shift(sell_range_down_date,1,type='lead'),symbol]
# 
# prices[symbol %in% prices[,.N,symbol][N>10,symbol],
#        ma5:= SMA(close, n = 5 ),symbol ]

rally_avg(prices,200)

prices=key_etfs(prices,low_corr_thresh=.33)

# Rally ETFs
# perf drawdown drawdown_days days_traded
# 1: 0.02313333     -3.4           536        1213
# year average drawdown total trades days_traded avg_days_held stocks_traded
# 1: 2008   0.057     -0.5   3.5     61          35      4.524590            12
# 2: 2009   0.007     -0.9   0.9    138          82      4.782609            10
# 3: 2010   0.005     -0.6   0.7    132         116      4.636364             7
# 4: 2011   0.030     -0.6   4.1    135          87      4.214815            11
# 5: 2012   0.009     -0.2   0.1     16          12      4.625000             7
# 6: 2013   0.021      0.0   0.1      3           3      2.666667             1
# 7: 2014   0.036      0.0   0.9     25          22      3.720000             6
# 8: 2015   0.022     -0.8   1.1     51          38      3.686275            10
# 9: 2016   0.006     -2.3   1.5    258         137      5.395349            17
# 10: 2017   0.016     -1.2   1.0     62          55      3.500000             5
# 11: 2018   0.044     -0.6   4.5    101          65      4.346535            11
# 12: 2019   0.036     -0.7   5.5    150          87      5.100000            10
# 13: 2020   0.012     -3.4   3.4    294         163      5.122449            36
# 14: 2021   0.019     -0.6   8.0    432         187      4.993056            30
# 15: 2022   0.027     -3.2   7.3    272         124      4.496324            34          35       28        45

setorder(prices, symbol, date)
prices[avg_volume>500000 & close>7 & 
         lead1sell_rally/lead1open<2 & 
         close<lag1high & sell_rally_day>2 &
         ((sell_rally_avg-avg_delta)/sell_rally_avg)>.018][
           order(-lever, lagging_corr_long,decreasing = F),head(.SD,3),date] %>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol))
#also works
prices[volume>75000 & close>7 & !short & 
         lead1sell_rally/lead1open<2 & 
         close<lag1high & sell_rally_day>2 &
         ((sell_rally_avg-avg_delta)/sell_rally_avg)>.018][
           order(-lever, (sell_rally_avg-avg_delta)/sell_rally_avg,decreasing = T),head(.SD,3),date] %>%
  with(performance(date,lead1sell_range_up/lead1open-1,lead1sell_range_update-date,symbol))

# revert ETFs
# perf drawdown drawdown_days days_traded
# 1: 0.02005556     -4.7           264        1566
# year average drawdown total trades days_traded avg_days_held stocks_traded
# 1: 2005   0.002      0.0   0.0      2           2      6.000000             1
# 2: 2006   0.047      0.0   0.2      5           3      2.400000             4
# 3: 2007   0.037      0.0   0.5     15          11      2.533333             8
# 4: 2008   0.022     -2.1   5.1    229          74      5.462882            42
# 5: 2009   0.023     -0.9   3.0    133          59      6.187970            40
# 6: 2010   0.021     -0.9   3.2    151          59      5.278146            24
# 7: 2011   0.010     -2.8   2.1    215          88      5.106977            55
# 8: 2012   0.016     -0.4   1.1     72          36      5.152778            19
# 9: 2013   0.027     -0.2   2.2     80          46      4.862500            14
# 10: 2014   0.029     -0.2   3.5    120          59      5.275000            23
# 11: 2015   0.018     -0.7   3.5    198         103      5.398990            33
# 12: 2016   0.029     -1.8   6.5    221         106      6.040724            39
# 13: 2017   0.027     -0.4   1.7     62          47      5.177419            11
# 14: 2018  -0.008     -4.4  -2.3    288         121      6.812500            44
# 15: 2019   0.015     -4.7   4.2    279         149      6.218638            28
# 16: 2020   0.010     -4.0   7.8    798         221      6.105263           123
# 17: 2021   0.007     -3.7   4.5    649         220      6.759630            67
# 18: 2022   0.029     -3.1  21.2    730         162      6.439726           100       85         113          71

prices[avg_volume>750000 & close>7 & (!lead1sell_rally/lead1open>2)  &
    (((close-low)/(high-low))<.2 ) & 
    ((high/close) > 1.075 
       |(!short & (avg_range/close) > .05 & (running_low == low) ) 
       #|(DEMA_s/lag1dema_s <.985) 
    )][order(lever, lagging_corr_long,decreasing=F),head(.SD,3),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol))


# perf drawdown drawdown_days days_traded
# 1: 0.02935294     -3.8           485        1048
# year average drawdown total trades days_traded avg_days_held stocks_traded
# 1: 2006   0.076      0.0   0.1      1           1      2.000000             1
# 2: 2007   0.045      0.0   0.3      6           6      2.000000             3
# 3: 2008   0.042     -1.8   3.8     91          33      5.274725            32
# 4: 2009   0.017     -0.1   0.4     24          15      3.666667            13
# 5: 2010   0.044     -0.2   4.7    106          47      4.386792            22
# 6: 2011   0.026     -2.1   4.1    158          71      4.063291            34
# 7: 2012   0.025     -0.2   0.8     33          15      3.515152            13
# 8: 2013   0.025     -0.2   1.4     58          28      3.724138            14
# 9: 2014   0.049     -0.3   2.4     49          25      5.040816            17
# 10: 2015   0.033     -1.1   3.9    118          58      4.601695            32
# 11: 2016   0.024     -3.8   4.5    190         103      5.726316            32
# 12: 2017  -0.015     -1.0  -0.6     39          37      7.512821             6
# 13: 2018   0.015     -3.1   4.2    280         121      5.389286            32
# 14: 2019   0.027     -2.7   4.1    150          95      5.500000            26
# 15: 2020   0.035     -1.2   9.8    283         108      4.462898           105
# 16: 2021   0.015     -2.6   4.9    324         151      6.145062            53
# 17: 2022   0.016     -2.3   7.0    441         134      5.163265            79           87         28        79         80       87

prices[avg_volume>500000 & close>7 & 
         avg_delta_short<.975 & lagging_corr_long> .35][
           order(lagging_corr_long),head(.SD,3),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol))
#also works
prices[ifelse(short, avg_volume>100000, avg_volume>500000) & close>7 & !(key_etf %in% c("USO","none")) &
         avg_delta_short<.98 & lagging_corr_long> .35][
           order(rownum),head(.SD,5),date]%>%
  with(performance(date,lead1sell_range_up/lead1open-1,lead1sell_rallydate-date,symbol))


