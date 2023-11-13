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

#                                                                   
# rally(prices,
#       sell_rule=function(dat){(dat$lag1close-dat$close)>=dat$avg_range*.5},
#       varname='sell_range_down')
# prices[,lead1sell_range_down:= shift(sell_range_down,1,type='lead'),symbol]
# prices[,lead1sell_range_downdate:= shift(sell_range_down_date,1,type='lead'),symbol]
# 
prices[symbol %in% prices[,.N,symbol][N>10,symbol],
       ma5:= SMA(close, n = 5 ),symbol ]

rally_avg(prices,200)

prices=key_etfs(prices,low_corr_thresh=.33)
 
# Rally ETFs
# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1:   0.0274 0.02294023     -3.3           687        1088       27
#    year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2008   0.068     -0.2   3.9     57          33       14      4.175439            10
# 2: 2009   0.006     -0.9   0.8    133          81       21      4.879699            10
# 3: 2010   0.012     -0.3   1.0     88          81        7      4.056818             9
# 4: 2011   0.033     -0.6   3.5    107          68       16      4.467290            13
# 5: 2012   0.012     -0.2   0.2     16          13        6      4.687500             6
# 6: 2013   0.016      0.0   0.1      4           5        3      3.250000             2
# 7: 2014   0.053      0.0   0.8     15          16        4      3.866667             4
# 8: 2015   0.032     -0.8   1.6     49          38       11      3.612245            13
# 9: 2016   0.010     -2.5   2.2    221         119       22      5.588235            16
# 10: 2017   0.014     -1.5   0.5     37          35        6      3.918919             5
# 11: 2018   0.056     -1.1   3.8     67          55       10      4.149254             8
# 12: 2019   0.037     -0.6   4.4    118          83       13      5.457627             9
# 13: 2020   0.009     -3.3   2.5    278         159       27      5.374101            36
# 14: 2021   0.023     -0.6   9.6    413         184       26      5.043584            25
# 15: 2022   0.030     -3.2   7.7    254         118       21      4.511811            29


setorder(prices, symbol, date)
prices[volume>500000 & close>7 & 
         lead1sell_rally/lead1open<2 & 
         close<lag1high & sell_rally_day>2 &
         avg_delta/sell_rally_avg<.982][
           order(lever, avg_volume,decreasing = T),head(.SD,3),date] %>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol,lead1sell_rallydate))

# revert ETFs
# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.02361111 0.01912335     -2.8           320        1366       29
#    year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2005   0.009      0.0   0.0      3           4        1      5.666667             2
# 2: 2006   0.040      0.0   0.1      2           3        1      4.000000             2
# 3: 2007   0.023      0.0   0.1      3           4        1      1.666667             3
# 4: 2008   0.008     -1.7   0.8    106          51       23      6.792453            39
# 5: 2009   0.033     -0.6   2.3     69          41       12      5.855072            27
# 6: 2010   0.046     -0.2   3.5     76          44       17      4.578947            18
# 7: 2011   0.004     -2.0   0.5    130          69       13      5.507692            35
# 8: 2012   0.012     -0.7   0.7     54          42        9      6.407407            15
# 9: 2013   0.030     -0.1   1.4     48          37        4      5.062500            15
# 10: 2014   0.045     -0.4   2.9     64          38       16      5.578125            22
# 11: 2015   0.030     -0.6   4.4    147         102       15      4.850340            29
# 12: 2016   0.031     -2.8   5.1    164         106       17      6.347561            30
# 13: 2017   0.042     -0.3   1.6     38          34        3      6.184211            11
# 14: 2018   0.004     -1.8   0.8    185         113       20      6.843243            40
# 15: 2019   0.018     -2.0   4.2    234         136       14      6.012821            30
# 16: 2020   0.014     -1.9   7.1    509         210       23      6.064833            84
# 17: 2021   0.014     -1.6   4.9    340         174       29      6.679412            47
# 18: 2022   0.022     -1.8   8.9    406         158       24      5.790640            73


prices[volume>500000 & close>7 & (lead1sell_rally/lead1open<2)  &
         (((close-low)/avg_range)<.15 ) & 
         (((high/close) > 1.075) | (avg_delta<.99)  
         )
    ][order( lagging_corr_long,decreasing=T),head(.SD,3),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol,lead1sell_rallydate))

# Corr long etfs

# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.02735294 0.02182994       -4           303        1109       28
#    year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2006   0.076      0.0   0.1      1           2        1      2.000000             1
# 2: 2007   0.028      0.0   0.2      6           7        1      2.833333             3
# 3: 2008   0.043     -1.1   3.0     70          36       23      4.828571            31
# 4: 2009   0.018     -0.1   0.3     18          13        4      4.166667            12
# 5: 2010   0.043     -0.2   3.9     90          49       18      4.288889            21
# 6: 2011   0.028     -1.0   3.7    133          75       14      4.060150            49
# 7: 2012   0.007     -0.3   0.2     27          18       10      4.148148            13
# 8: 2013   0.022     -0.3   1.2     53          30       12      3.773585            15
# 9: 2014   0.053     -0.3   2.9     55          31       16      5.472727            26
# 10: 2015   0.026     -0.9   3.0    117          69       19      4.529915            39
# 11: 2016   0.018     -4.0   3.2    176         108       28      5.994318            32
# 12: 2017   0.012     -0.8   0.6     50          44       13      5.800000            13
# 13: 2018   0.006     -2.2   1.3    215         114       24      5.906977            40
# 14: 2019   0.026     -1.9   4.2    158         107       15      5.500000            30
# 15: 2020   0.025     -2.9   5.7    228         117       19      4.868421            79
# 16: 2021   0.018     -1.2   5.1    289         153       21      5.961938            46
# 17: 2022   0.016     -2.0   5.3    325         136       22      5.224615            77

prices[volume>500000 & close>7 & 
         (avg_delta_short<.975) & lagging_corr_long> .35][
           order(avg_volume),head(.SD,3),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol, lead1sell_rallydate))
