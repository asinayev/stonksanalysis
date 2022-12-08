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


prices[,MACD:=NULL ]
prices[(symbol %in% prices[!is.na(close),.N,symbol][N>26,symbol]) & !is.na(close),
       MACD:=EMA(close ,n = 12, align='right',fill=NA)/
             EMA(close ,n = 26, align='right',fill=NA),symbol ]
prices[(symbol %in% prices[!is.na(MACD),.N,symbol][N>10,symbol]) & !is.na(MACD),
       MACD_slow:=EMA(MACD ,n = 9, align='right',fill=NA),symbol ]

prices[,RSI2:=NULL ]
prices[(symbol %in% prices[!is.na(close-lag1close),.N,symbol][N>5,symbol]) & !is.na(close-lag1close),
       RSI2:=SMA(pmax(0, close-lag1close) ,n = 2, align='right',fill=NA)/
         SMA(pmax(0, lag1close-close) ,n = 2, align='right',fill=NA),symbol ]

prices[,PDM:=NULL ]
prices[(symbol %in% prices[!is.na(close-lag1close),.N,symbol][N>5,symbol]) & !is.na(close-lag1close),
       PDM:=SMA(pmax(0, high-lag1high) ,n = 3, align='right',fill=NA),
       symbol ]
prices[,NDM:=NULL ]
prices[(symbol %in% prices[!is.na(close-lag1close),.N,symbol][N>5,symbol]) & !is.na(close-lag1close),
       NDM:=SMA(pmax(0, lag1low-low  ) ,n = 3, align='right',fill=NA),
       symbol ]
prices[,ADX:=NULL ]
prices[(symbol %in% prices[!is.na((PDM-NDM)/(PDM+NDM)),.N,symbol][N>30,symbol]) & !is.na((PDM-NDM)/(PDM+NDM)),
       ADX:=EMA((PDM-NDM)/(PDM+NDM) ,n = 30, align='right',fill=NA),
       symbol ]


lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)
prices[,lead1sell_rally:= shift(sell_rally,1,type='lead'),symbol]
prices[,lead1sell_rallydate:= shift(sell_rally_date,1,type='lead'),symbol]

rally(prices,
      sell_rule=function(dat){(dat$close-dat$lag1close)>=dat$avg_range*.5},
      varname='sell_range_up')
prices[,lead1sell_range_up:= shift(sell_range_up,1,type='lead'),symbol]
prices[,lead1sell_range_update:= shift(sell_range_up_date,1,type='lead'),symbol]
                                                                  
rally(prices,
      sell_rule=function(dat){(dat$lag1close-dat$close)>=dat$avg_range*.5},
      varname='sell_range_down')
prices[,lead1sell_range_down:= shift(sell_range_down,1,type='lead'),symbol]
prices[,lead1sell_range_downdate:= shift(sell_range_down_date,1,type='lead'),symbol]

prices[symbol %in% prices[,.N,symbol][N>10,symbol],
       ma5:= SMA(close, n = 5 ),symbol ]
rally(prices,
      sell_rule=function(dat){(dat[,ma5<shift(ma5,n=1,type='lag') & shift(ma5,n=1,type='lag')>shift(ma5,n=2,type='lag')] ) },
      varname='sell_slope')
prices[,lead1sell_slope:= shift(sell_slope,1,type='lead'),symbol]
prices[,lead1sell_slopedate:= shift(sell_slope_date,1,type='lead'),symbol]

rally_avg(prices,200)

prices=key_etfs(prices,low_corr_thresh=.33)

# Rally ETFs
# perf drawdown days_traded
# 1: 0.02186667     -4.6        1256
# year average drawdown total trades days_traded avg_days_held stocks_traded
# 1: 2008   0.054     -0.5   3.3     62          36      4.467742            14
# 2: 2009   0.006     -0.9   0.9    140          82      4.592857            11
# 3: 2010   0.011     -0.3   1.3    117          96      3.777778             8
# 4: 2011   0.029     -0.6   4.1    141          84      4.219858            11
# 5: 2012   0.018      0.0   0.3     19          15      2.157895             7
# 6: 2013   0.030     -0.2   0.6     19          16      6.684211             4
# 7: 2014  -0.009     -1.5  -0.4     47          37      3.957447             9
# 8: 2015   0.049     -1.3   3.3     68          47      3.882353            15
# 9: 2016   0.007     -4.4   2.0    267         124      5.827715            23
# 10: 2017   0.019     -2.5   1.5     79          68      3.481013            10
# 11: 2018   0.033     -1.2   3.4    103          83      4.466019            14
# 12: 2019   0.016     -2.4   3.0    188         131      5.340426            12
# 13: 2020   0.016     -4.6   4.5    283         139      4.936396            40
# 14: 2021   0.023     -0.5   9.0    399         173      4.503759            35
# 15: 2022   0.026     -3.3   7.1    274         125      4.489051            35       28        45

setorder(prices, symbol, date)
prices[volume>75000 & close>7 & ifelse(short, (key_etf %in% c("AVUV","JEPI","WCLD")), T) &
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
# perf drawdown days_traded
# 1: 0.02105556     -2.7         649
# year average drawdown total trades days_traded avg_days_held stocks_traded
# 1: 2005   0.009      0.0   0.0      3           3      5.666667             2
# 2: 2006   0.013      0.0   0.0      1           1      5.000000             1
# 3: 2007   0.014      0.0   0.1     10          10      3.900000             5
# 4: 2008   0.012     -2.7   1.5    125          41      6.648000            56
# 5: 2009   0.036     -0.7   2.5     71          34      7.042254            34
# 6: 2010   0.053     -0.2   3.4     64          30      4.718750            36
# 7: 2011   0.009     -2.2   0.8     97          41      5.298969            42
# 8: 2012   0.023     -0.1   0.8     34          17      5.029412            17
# 9: 2013   0.011     -0.2   0.4     33          21      6.242424            22
# 10: 2014   0.010     -1.6   0.6     59          29      5.949153            29
# 11: 2015   0.037     -0.8   3.6     97          55      5.463918            41
# 12: 2016   0.041     -1.3   5.0    121          58      6.801653            38
# 13: 2017  -0.012     -0.6  -0.3     26          15      6.500000            17
# 14: 2018   0.015     -1.7   1.8    117          52      6.923077            45
# 15: 2019   0.041     -0.4   2.1     51          33      4.745098            20
# 16: 2020   0.022     -1.8   5.2    232          83      5.547414           108
# 17: 2021   0.026     -0.6   3.7    142          71      6.253521            61
# 18: 2022   0.019     -1.7   3.1    168          55      6.517857            71

prices[volume>75000 & close>7 & !(key_etf %in% c("USO","none")) &
         lead1sell_rally/lead1open<2 & 
    (((close-low)/(high-low))<.05 ) & 
    ((high/close) > 1.075 |
       (!short & (high/close) > 1.05 & (running_low == low | MACD_slow<.975) 
       ) 
    )][order(lever, lagging_corr_long,decreasing=F),head(.SD,5),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol))


# perf drawdown days_traded
# 1: 0.02735294     -4.2        1298
# year average drawdown total trades days_traded avg_days_held stocks_traded
# 1: 2006   0.076      0.0   0.1      1           1      2.000000             1
# 2: 2007   0.045      0.0   0.3      6           6      2.000000             3
# 3: 2008   0.037     -1.9   3.4     91          33      5.241758            36
# 4: 2009   0.017     -0.1   0.4     24          15      3.666667            13
# 5: 2010   0.044     -0.2   6.6    149          59      4.785235            29
# 6: 2011   0.023     -2.1   4.4    195          78      4.153846            49
# 7: 2012   0.027     -0.2   1.1     39          19      3.538462            16
# 8: 2013   0.014     -0.6   1.0     75          43      4.120000            17
# 9: 2014   0.048     -0.3   2.5     52          28      5.269231            17
# 10: 2015   0.025     -1.1   4.0    163          89      4.699387            37
# 11: 2016   0.017     -4.2   4.5    263         131      5.764259            36
# 12: 2017  -0.002     -1.7  -0.2     96          75      6.687500             9
# 13: 2018   0.014     -2.9   4.9    347         146      5.645533            44
# 14: 2019   0.024     -3.3   6.9    290         147      5.548276            37
# 15: 2020   0.025     -2.0   8.1    327         129      4.758410           106
# 16: 2021   0.012     -2.2   4.3    357         163      6.302521            55
# 17: 2022   0.019     -2.2   8.4    449         136      5.031180            87         28        79         80       87

prices[ifelse(short, avg_volume>100000, avg_volume>500000) & close>7 & !(key_etf %in% c("USO","none")) &
         avg_delta_short<.975 & lagging_corr_long> .35][
           order(lagging_corr_long),head(.SD,5),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol))
#also works
prices[ifelse(short, avg_volume>100000, avg_volume>500000) & close>7 & !(key_etf %in% c("USO","none")) &
         avg_delta_short<.975 & lagging_corr_long> .35][
           order(rownum),head(.SD,5),date]%>%
  with(performance(date,lead1sell_range_up/lead1open-1,lead1sell_rallydate-date,symbol))
