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


rally(prices,
      sell_rule=function(dat){dat$lag1close>dat$lag1low+.8*(dat$lag1high-dat$lag1low) },
      varname='sell_lowclose',
      sell_close=F)
prices[,lead1sell_lowclose:= shift(sell_lowclose,1,type='lead'),symbol]
prices[,lead1sell_combined:=ifelse(short,lead1sell_lowclose,lead1sell_rally)]

prices[,lag1RSI:= shift(RSI_short,1,type='lag'),symbol]
rally(prices,
      sell_rule=function(dat){dat$RSI_short>50 & dat$lag1RSI<50},
      varname='sell_rsi')
prices[,lead1sell_rsi:= shift(sell_rsi,1,type='lead'),symbol]
prices[,lead1sell_rsi_date:= shift(sell_rsi_date,1,type='lead'),symbol]
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
# 
# Rally ETFs
#    avg_year  avg_trade drawdown drawdown_days days_traded
# 1:   0.0274 0.02294023     -3.3           687        1073
# year average drawdown total trades days_traded avg_days_held stocks_traded
# 1: 2008   0.068     -0.2   3.9     57          32      4.175439            10
# 2: 2009   0.006     -0.9   0.8    133          80      4.879699            10
# 3: 2010   0.012     -0.3   1.0     88          80      4.056818             9
# 4: 2011   0.033     -0.6   3.5    107          67      4.467290            13
# 5: 2012   0.012     -0.2   0.2     16          12      4.687500             6
# 6: 2013   0.016      0.0   0.1      4           4      3.250000             2
# 7: 2014   0.053      0.0   0.8     15          15      3.866667             4
# 8: 2015   0.032     -0.8   1.6     49          37      3.612245            13
# 9: 2016   0.010     -2.5   2.2    221         118      5.588235            16
# 10: 2017   0.014     -1.5   0.5     37          34      3.918919             5
# 11: 2018   0.056     -1.1   3.8     67          54      4.149254             8
# 12: 2019   0.037     -0.6   4.4    118          82      5.457627             9
# 13: 2020   0.009     -3.3   2.5    278         158      5.374101            36
# 14: 2021   0.023     -0.6   9.6    413         183      5.043584            25
# 15: 2022   0.030     -3.2   7.7    254         117      4.511811            29       28        45

setorder(prices, symbol, date)
prices[volume>500000 & close>7 & 
         lead1sell_rally/lead1open<2 & 
         close<lag1high & sell_rally_day>2 &
         ((sell_rally_avg-avg_delta)/sell_rally_avg)>.018][
           order(lever, avg_volume,decreasing = T),head(.SD,3),date] %>%
  with(performance(date,lead1sell_combined/lead1open-1,lead1sell_rallydate-date,symbol))

# revert ETFs
#     avg_year  avg_trade drawdown drawdown_days days_traded
# 1: 0.02355556 0.01971935     -3.5           251        1360
# year average drawdown total trades days_traded avg_days_held stocks_traded
# 1: 2005   0.002      0.0   0.0      2           2      6.000000             1
# 2: 2006   0.067      0.0   0.1      1           1      3.000000             1
# 3: 2007   0.012      0.0   0.1      5           5      2.800000             5
# 4: 2008   0.015     -2.0   2.0    136          64      5.764706            41
# 5: 2009   0.030     -0.7   3.0     99          51      6.121212            22
# 6: 2010   0.036     -0.4   3.3     92          47      4.880435            14
# 7: 2011   0.009     -2.1   1.3    140          73      5.250000            28
# 8: 2012   0.022     -0.1   0.8     36          22      4.638889            11
# 9: 2013   0.025     -0.1   1.1     43          34      4.720930             6
# 10: 2014   0.044     -0.1   3.0     69          45      4.869565            14
# 11: 2015   0.026     -0.8   3.2    124          76      5.209677            21
# 12: 2016   0.038     -1.4   5.8    154          96      6.370130            19
# 13: 2017   0.026     -0.4   1.3     51          41      5.098039            11
# 14: 2018  -0.004     -3.5  -0.8    210         113      6.714286            29
# 15: 2019   0.021     -2.9   4.2    203         130      5.620690            17
# 16: 2020   0.021     -1.6  10.6    507         209      5.686391            86
# 17: 2021   0.017     -1.0   6.7    392         192      6.375000            43
# 18: 2022   0.017     -2.5   7.7    444         159      6.168919            64     113          71


prices[avg_volume>1000000 & close>7 & (lead1sell_combined/lead1open<2)  &
         (((close-low)/avg_range)<.2 ) & 
         (((high/close) > 1.075) | (avg_delta<.99) | 
            (!short & lever &  (MACD_slow<.975 | running_low == low ) ) 
         )
    ][order( lagging_corr_long,decreasing=T),head(.SD,3),date]%>%
  with(performance(date,lead1sell_combined/lead1open-1,lead1sell_rallydate-date,symbol))

# Corr long etfs

# avg_year  avg_trade drawdown drawdown_days days_traded
# 1: 0.02852941 0.02370868     -4.5           485        1048
# year average drawdown total trades days_traded avg_days_held stocks_traded
# 1: 2006   0.076      0.0   0.1      1           1      2.000000             1
# 2: 2007   0.045      0.0   0.3      6           6      2.000000             3
# 3: 2008   0.040     -1.4   2.9     72          33      5.013889            26
# 4: 2009   0.017     -0.1   0.4     23          15      3.782609            12
# 5: 2010   0.043     -0.2   3.7     87          47      4.367816            20
# 6: 2011   0.033     -0.8   4.2    125          71      4.144000            34
# 7: 2012   0.016     -0.2   0.4     25          15      3.720000            11
# 8: 2013   0.027     -0.2   1.3     48          28      3.916667            11
# 9: 2014   0.045     -0.3   1.9     43          25      5.348837            16
# 10: 2015   0.027     -1.1   2.7    100          58      4.910000            23
# 11: 2016   0.018     -4.5   3.0    166         103      5.813253            24
# 12: 2017  -0.015     -1.0  -0.6     39          37      7.512821             6
# 13: 2018   0.014     -2.5   3.2    236         121      5.432203            32
# 14: 2019   0.029     -2.2   4.1    143          95      5.489510            25
# 15: 2020   0.032     -1.4   6.9    217         108      4.663594            71
# 16: 2021   0.017     -1.2   4.8    287         151      6.083624            42
# 17: 2022   0.021     -2.0   6.6    318         134      5.000000            67     80       87

prices[avg_volume>500000 & close>7 & 
         (avg_delta_short<.975) & lagging_corr_long> .35][
           order(avg_volume),head(.SD,3),date]%>%
  with(performance(date,lead1sell_combined/lead1open-1,lead1sell_rallydate-date,symbol))
# also works
prices[ifelse(short, avg_volume>100000, avg_volume>500000) & close>7 & !(key_etf %in% c("USO","none")) &
         avg_delta_short<.98 & lagging_corr_long> .35][
           order(rownum),head(.SD,5),date]%>%
  with(performance(date,lead1sell_range_up/lead1open-1,lead1sell_rallydate-date,symbol))
