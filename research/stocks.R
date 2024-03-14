require(tidyquant)
require(data.table)
require(rpart)
require(ggplot2)

setwd('~/stonksanalysis')
source("polygon.R", local=T)
source("implement/features.R", local=T)
source("research/performance.R", local=T)
POLYKEY = Sys.getenv('POLYGONKEY')

# Get data from polygon instead

# prices=lapply(Sys.Date()-365*18:1, sampled_data, key=POLYKEY, ticker_type='CS', details=T, financials=F) %>%   
#   rbindlist(fill=T) %>%
#   dplyr::rename(symbol=stock, close=AdjClose, date=Date)
# spy_prices=stock_history('SPY', '2004-01-01', Sys.Date(), key=POLYKEY,check_ticker=F) %>%
#   dplyr::rename(symbol=stock, close=AdjClose, date=Date)
# spy_prices[,c("lag1close", "lag2close", "lead1close"):=shift(close, n = c(1:2,-1), type = "lag"),symbol]
# spy_prices[,c("lag1open",  "lag2open", "lead1open"):=shift(open,  n = c(1:2,-1), type = "lag"),symbol]
# prices = merge(prices,spy_prices[,.(date,spy_future_night_delta = lead1open/close, spy_day_delta=close/open)],all.x=T)

prices=fread("~/datasets/stock_prices_15y.csv")
setorder(prices, symbol, date)
prices = prices[!is.na(volume) & !is.na(close) & !is.na(open)]

#prices=get_financials(prices,identifier='symbol')
lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)
prices[,lead1sell_rally:= shift(sell_rally,1,type='lead'),symbol]
prices[,lead1sell_rallydate:= shift(sell_rally_date,1,type='lead'),symbol]

prices[(symbol %in% prices[!is.na(close),.N,symbol][N>106,symbol]) & !is.na(close),
       MACD:=EMA(close ,n = 5, align='right',fill=NA)/
         EMA(close ,n = 100, align='right',fill=NA),symbol ]
prices[(symbol %in% prices[!is.na(MACD),.N,symbol][N>11,symbol]) & !is.na(MACD),
       MACD_slow:=EMA(MACD ,n = 10, align='right',fill=NA),symbol ]
prices[,lag1MACD:= shift(MACD,1,type='lag'),symbol]
prices[,lag1MACD_slow:= shift(MACD_slow,1,type='lag'),symbol]


prices[(symbol %in% prices[,.N,symbol][N>26,symbol]) ,
       avg_range_p:=SMA(high/low-1 ,n = 25, align='right',fill=NA),symbol ]

rally(prices,
      sell_rule=function(dat){dat$lag1close<dat$lag1low+.2*(dat$lag1high-dat$lag1low) },
      varname='sell_lowclose',
      sell_close=F)
prices[,lead1sell_lowclose:= shift(sell_lowclose,1,type='lead'),symbol]
prices[,lead1sell_lowclosedate:= shift(sell_lowclose_date,1,type='lead'),symbol]


prices[symbol %in% prices[,.N,symbol][N>100,symbol]
       ,max_volume:= zoo::rollapply(volume,max,width=100, align='right',fill=NA),symbol ]
prices[symbol %in% prices[,.N,symbol][N>25,symbol]
       ,avg_vp:= frollmean(close*volume ,n = 25, align='right',fill=NA),symbol ]
prices[order(avg_vp,decreasing=T),    vp_order:=seq_len(.N),date]
prices[order(market_cap,decreasing=T),cap_order:=seq_len(.N),date]


#####bandlong
# prices[!is.na(lag1close),
#        c('lower','avg','upper','pctB'):= data.frame(BBands(lag1close, n = 30, EMA, sd=2.5)),
#        symbol ]
# prices[(close<low*1.01 | close<low_running) & day_delta<.8 & close>7
#        & lag1volume>75000, 
#         .(mean(lead1open/close, na.rm=T), median(lead1open/close,na.rm=T),.N),
#        year(date)][order(year)]

#At close, buy stocks that fell 15% and closed at the day's low or 50D low of close prices
#####

#####updown
# prices[
#   open/lag1close> 1.05 & close/open<.9  & close>7 & 
#     volume>75000  & volume*lag1close<1000000
#   ,.(mean(lead1open/close, na.rm=T), .N), 
#   .(year(date))][order(year)] 

#At close, buy stocks that climbed last night and fell today, setting limit to 95% of the open
#####

#####updownmorn
# perf drawdown days_traded
# 1: 0.01705263     -0.5        2444
# year average drawdown total trades days_traded avg_days_held stocks_traded
# 1: 2004   0.008     -0.1   0.3     32          21             1            28
# 2: 2005   0.010     -0.1   1.2    117          84             1            93
# 3: 2006   0.007     -0.2   1.2    168         107             1           106
# 4: 2007   0.009     -0.3   1.5    158          92             1            99
# 5: 2008   0.019     -0.2   4.4    229         113             1           134
# 6: 2009   0.023     -0.1   6.7    286         136             1           171
# 7: 2010   0.015     -0.2   2.3    150          99             1           109
# 8: 2011   0.035     -0.2   3.6    103          76             1            79
# 9: 2012   0.015     -0.1   1.8    121          86             1            75
# 10: 2013   0.012     -0.2   2.0    164         121             1            99
# 11: 2014   0.017     -0.1   3.3    192         130             1           109
# 12: 2015   0.010     -0.5   2.0    201         130             1           107
# 13: 2016   0.018     -0.4   3.9    217         122             1           121
# 14: 2017   0.016     -0.3   5.4    335         154             1           212
# 15: 2018   0.023     -0.3  13.7    598         210             1           363
# 16: 2019   0.021     -0.4  14.2    681         225             1           379
# 17: 2020   0.025     -0.3  17.7    715         202             1           372
# 18: 2021   0.019     -0.3  11.1    570         209             1           328
# 19: 2022   0.022     -0.4   8.5    391         127             1           249
prices[
  close/open>1.025 & spy_future_night_delta>.99 & 
    volume%between%c(10000,20000) & close>5 & lead1open/close<.975]%>%
  with(performance(date,lead1close/lead1open-1,1,symbol))

prices[
  volume/avg_volume <.75 & spy_future_night_delta>.99 & 
    volume%between%c(10000,20000) & close>5 & lead1open/close<.975]%>%
  with(performance(date,lead1close/lead1open-1,1,symbol))

prices[
  (volume/avg_volume <.75 | close/open>1.025) & spy_future_night_delta>.99 & 
    volume%between%c(10000,20000) & close>5 & (lead1open/close)<.975]%>%
  with(performance(date,(lead1close/lead1open)-1,1,symbol))

#At open, buy stocks that climbed yesterday but fell overnight today unless the index fell overnight
#####

#####overbought

# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.03557895 0.04292958     -7.6           622        1156       21
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2004   0.033      0.0   0.1      2           3        2             1             2
# 2: 2005   0.016      0.0   0.1      5           6        2             1             5
# 3: 2006  -0.026     -0.5  -0.3     13          14        2             1            12
# 4: 2007   0.062     -0.1   1.6     25          22        6             1            21
# 5: 2008   0.046     -1.0   6.4    140          75       13             1           113
# 6: 2009   0.005     -2.0   0.8    151          93       15             1           111
# 7: 2010   0.026     -1.3   0.9     33          33        4             1            30
# 8: 2011   0.027     -0.8   0.8     30          24        3             1            25
# 9: 2012   0.053     -0.3   1.7     32          30        4             1            24
# 10: 2013   0.043     -0.5   0.9     21          20        4             1            17
# 11: 2014   0.047     -0.8   1.6     33          29        8             1            25
# 12: 2015   0.022     -1.6   1.4     62          55        5             1            46
# 13: 2016  -0.029     -3.4  -2.4     83          61       10             1            59
# 14: 2017   0.099     -2.9   9.5     96          69       11             1            77
# 15: 2018   0.076     -2.0  11.9    158         112       15             1           115
# 16: 2019   0.031     -1.4   4.6    151         107       11             1           114
# 17: 2020   0.060     -2.7  21.8    365         184       16             1           271
# 18: 2021   0.036     -7.6   9.9    275         157       21             1           189
# 19: 2022   0.049     -1.5   4.9    100          62        8             1            85

prices[
  (close/open)>1.2 & close>7 & (open/lag1close)>1  &
    vp_order<3000][order(close*volume,decreasing=T),.SD[1:3],date]%>% 
  with(performance(date,1-lead1sell_lowclose/lead1open,1,symbol,lead1sell_lowclosedate))


# At open, sell stocks that climbed yesterday too much
#####


###### volumeshort
# prices[lag1volume/volume_avg>7.5 & lag1_day_delta>.975 & night_delta>1.01  & open>7.5 &
#          lag1volume%between%c(10000,100000),
#        .(mean(day_delta,na.rm=T),.N), year(date)][order(year)]
#
######


#####volumelong -- new version

# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.01194444 0.01277696     -2.2          1267         970       16
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2005   0.015     -0.1   0.9     59          50        4             1            57
# 2: 2006   0.008     -0.4   0.5     64          49        6             1            53
# 3: 2007   0.021     -0.1   1.8     87          63        8             1            78
# 4: 2008   0.028     -0.9   4.9    179         109       16             1           160
# 5: 2009   0.012     -0.9   1.0     81          61        7             1            70
# 6: 2010   0.015     -0.4   1.0     70          50        8             1            67
# 7: 2011   0.001     -0.6   0.1     87          65       14             1            71
# 8: 2012   0.013     -0.5   0.9     68          52        9             1            63
# 9: 2013   0.014     -0.1   1.2     85          58        9             1            80
# 10: 2014   0.010     -0.4   0.9     93          66       14             1            78
# 11: 2015   0.024     -0.3   1.9     77          55        9             1            70
# 12: 2016   0.017     -0.1   1.2     74          56        8             1            69
# 13: 2017   0.011     -0.1   0.3     25          25        3             1            22
# 14: 2018  -0.002     -0.7  -0.2     98          65       14             1            85
# 15: 2019   0.014     -0.6   0.4     26          25        3             1            25
# 16: 2020  -0.006     -2.2  -0.4     67          41       13             1            63
# 17: 2021  -0.004     -1.2  -0.3     63          43        8             1            60
# 18: 2022   0.024     -0.9   1.2     51          37        6             1            47

prices[lead1sell_rally/lead1open<1.5 & close>5 & volume>100000 & #exclude stuff that can't be traded
         (volume>=max_volume & avg_delta_short<.99) & #big down movement recently and consolidated today
         (((close-low)/avg_range)<.2 ) & 
         (log(vp_order)-log(cap_order))>.35 ][ #stock is boring
           order(avg_delta_short),head(.SD,3),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,1,symbol,lead1sell_rallydate))

## volumelong -- incorporated into updownmorn
# prices[lag1volume/volume_avg <.75 & night_delta< .97  & close>5 & 
#          lag1volume%between%c(10000,100000),
#        .(mean(day_delta,na.rm=T),.N), year(date)][order(year)]
# ###### works for ADRCs with less conservative threshold
# prices[lag1volume/volume_avg <1 & night_delta< .96 & 
#          volume_avg*lag1close>100000 & volume_avg*lag1close<1000000 & 
#          volume_avg>50000,
#        .(mean(day_delta,na.rm=T),.N), year(date)][order(year)]
######



######
# Regression strategy
sq=function(x)x^2
prices[,reg_predict := as.numeric(32)]
prices[,reg_predict := NA]
regression_features(prices)
for (yr in 2008:2021 ){
  IS = prices[year(date) %between% c(yr-3, yr-1) & volume>75000 & close>7 ]
  lm1 = lm(future_day_delta ~
             day_delta + night_delta + day_fall + day_rise 
           ,IS, weights = (IS$date-min(IS$date))/as.integer(max(IS$date-min(IS$date)))
  )
  print(yr)
  print(round(lm1$coefficients,3))
  prices[year(date)==yr, reg_predict:=predict(lm1,data.frame(.SD))  ]
  gc()
}


IS = prices[date>Sys.Date()-3*365 & date<Sys.Date()-30 & volume>75000 & close>7]
lm1 = lm(future_day_delta ~
           day_delta + night_delta + day_fall + day_rise
         ,IS, weights = (IS$date-min(IS$date))/as.integer(max(IS$date-min(IS$date)))
)
prices[year(date)==2022,
       reg_predict:=predict(lm1,data.frame(.SD))  ]

prices[,reg_predict:=ifelse(is.na(reg_predict),1,reg_predict)]
prices[volume>75000 & close>7,threshold:=pmin(quantile(reg_predict,.001,type=1),.995), date]

# year        V1   N
# 1: 2015 1.0015948 320
# 2: 2016 1.0033732 153
# 3: 2017 0.9783223  34
# 4: 2018 0.9636028 101
# 5: 2019 0.9748256 652
# 6: 2020 0.9776544 733
# 7: 2021 0.9858612 677
# 8: 2022 0.9796050  85
prices[reg_predict<threshold  & #!day_delta>1.2 &
         volume>75000 & close>7]%>% 
  with(performance(date,1-lead1close/lead1open,1,symbol))

prices[!is.na(future_day_delta) & reg_predict<threshold  & volume>75000 & close>7][order(date, symbol)][
  ,.(date, MA = EMA(future_day_delta,na.rm=T,50))] %>% with(plot(date, MA, type='l', ylim=c(.8,1.2)))
x_ <- c(1, .99, 1.01,.95,1.05) %>% lapply( function(x)abline(h=x))


# Overnight strategy makes money over 50 trades (either long or short) with few exceptions
# There must be a way to get this to work by lowering thresholds
window = 100
prices[,lagging_corr_long:=NULL]
prices[symbol %in% prices[days_around>window, unique(symbol)], 
       lagging_corr_long:=
         runCor( close/open, open/lag1close, window),
       symbol]

min_corr = .45
prices[lead1open/close<.98 & lagging_corr_long< -min_corr & spy_future_night_delta>.99 & 
         volume%between%c(10000,50000) & close>7]%>% 
  with(performance(date,lead1close/lead1open-1,1,symbol))

prices[lead1open/close>1.02 & lagging_corr_long< -min_corr & 
         volume%between%c(10000,50000) & close>7]%>% 
  with(performance(date,1-lead1close/lead1open,1,symbol))

prices[future_night_delta<.96 & lagging_corr< -.4 & !is.na(future_day_delta_ltd) & log(volume_avg+1) %between% c(10,25)][order(date, symbol)][ ,
                                                                                                                                               .(date, MA = EMA(future_day_delta_ltd,na.rm=T,50))] %>% with(plot(date, MA, type='l', ylim=c(.8,1.2)))
abline(h=1)
abline(h=0.99)
abline(h=1.01)

prices[future_night_delta>1.04 & lagging_corr< -.4 & !is.na(future_day_delta_ltd) & log(volume_avg+1) %between% c(10,25)][order(date, symbol)][ 
  ,.(date, MA = EMA(future_day_delta_ltd,na.rm=T,50))] %>% with(plot(date, MA, type='l', ylim=c(.8,1.2)))
abline(h=1)
abline(h=1.01)
abline(h=0.99)



###### Correlated long and short
rally_avg(prices,100)
# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.03344444 0.02514558     -7.4          1035        1006       48
#    year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2005   0.019     -0.4   0.6     33          31        7      3.787879            10
# 2: 2006   0.068     -0.2   1.8     27          17       14      3.148148            11
# 3: 2007   0.013     -5.9   1.3    102          52       22      5.549020            31
# 4: 2008   0.009     -6.8   3.3    351         145       48      6.612536           138
# 5: 2009   0.032     -7.4   2.9     91          43       31      6.307692            32
# 6: 2010   0.045     -2.5   0.7     16          17        3      2.625000             6
# 7: 2011   0.066     -1.8   2.4     36          27        9      3.416667            21
# 8: 2012   0.081     -0.1   2.2     27          26        9      5.703704             9
# 9: 2013   0.067      0.0   0.6      9          10        3      3.777778             6
# 10: 2014   0.035     -1.2   2.7     79          56       13      5.518987            21
# 11: 2015   0.042     -2.0   4.0     95          66       34      6.347368            23
# 12: 2016   0.018     -3.1   1.8    102          59       42      9.225490            27
# 13: 2017   0.010     -2.3   0.2     22          21        7      6.909091             6
# 14: 2018   0.018     -1.9   1.6     90          67       18      7.266667            35
# 15: 2019  -0.010     -3.0  -0.8     86          69       22      8.965116            28
# 16: 2020   0.060     -6.3  14.1    234          84       37      5.636752           104
# 17: 2021   0.030     -1.9   8.2    276         138       21      5.608696            86
# 18: 2022  -0.001     -2.8  -0.1    213          78       40      7.276995            65

prices[close>7 & volume>500000 & 
         close<lag1high & sell_rally_day>6 & 
         avg_delta<.975][
         ][order(high/close, decreasing=T),head(.SD,5),date] %>%
  with(performance(date,
                   lead1sell_rally/lead1open-1,
                   lead1sell_rallydate-date,symbol,
                   lead1sell_rallydate))

# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.04044444 0.04696212     -6.8          1414         839       27
#    year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2005   0.018      0.0   0.1      4           5        2      6.250000             3
# 2: 2006  -0.006     -0.1   0.0      4           5        2      5.750000             3
# 3: 2007  -0.001     -0.3   0.0     12          13        3     13.416667             9
# 4: 2008   0.087     -0.5   6.9     79          40       18      5.708861            58
# 5: 2009   0.051     -1.0   2.9     58          42       11      6.327586            45
# 6: 2010   0.031     -1.1   0.6     18          19        4      7.111111            13
# 7: 2011  -0.013     -0.8  -0.3     23          21        8     13.217391            13
# 8: 2012   0.049     -0.8   0.3      6           7        4      5.000000             5
# 9: 2013   0.120     -0.5   1.9     16          16        3     12.187500            11
# 10: 2014   0.073     -0.5   1.8     25          20        5      4.800000            14
# 11: 2015   0.057     -0.5   2.0     35          25        9      6.857143            26
# 12: 2016  -0.044     -2.9  -2.3     52          36       16      6.423077            28
# 13: 2017   0.034     -3.3   2.2     64          45        9      7.078125            49
# 14: 2018   0.107     -1.2   9.8     91          72        8      6.725275            64
# 15: 2019   0.051     -0.6   5.5    109          81       10      6.862385            82
# 16: 2020   0.073     -2.1  29.2    401         175       24      7.588529           258
# 17: 2021   0.003     -6.8   0.9    293         147       27      6.559727           203
# 18: 2022   0.038     -4.0   4.2    109          70       19      7.706422            69

prices[close>7 & volume>500000 & 
         close>lag1high & sell_rally_day<2 & 
         avg_delta_short>1.1][
         ][order(days_around, decreasing=T),head(.SD,5),date]%>%
  with(performance(date,
                   1-lead1sell_lowclose/lead1open,
                   lead1sell_lowclosedate-date,symbol,
                   lead1sell_lowclosedate))


############
# nightbot
# 
# prices[close>5 & volume>100000 & lead1open/close>1.15 & spy_future_night_delta<1.005 ]%>%
#   with(performance(date,1-lead1sell_lowclose/lead1open,
#                    1,symbol))

##############
# megacap
# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.01036842 0.01034606     -1.6           239        1562       18
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2004   0.017      0.0   0.3     18          14        4      3.888889            16
# 2: 2005   0.006     -0.4   1.3    206         117       17      6.126214            50
# 3: 2006   0.009     -0.1   0.9    102          67       10      5.754902            42
# 4: 2007   0.016     -0.1   1.9    122          61       11      4.967213            46
# 5: 2008   0.024     -0.6   6.4    267         124       18      5.419476            50
# 6: 2009   0.020     -1.6   3.1    156          81       13      5.358974            43
# 7: 2010   0.014     -0.2   1.8    128          69       12      4.921875            46
# 8: 2011   0.006     -0.6   0.9    155          80       15      5.754839            44
# 9: 2012   0.003     -0.4   0.4    129          74       18      6.069767            44
# 10: 2013   0.004     -0.6   0.6    158          88       17      5.816456            42
# 11: 2014   0.008     -0.3   1.4    181          95       18      5.071823            47
# 12: 2015   0.007     -0.5   1.2    179         103       15      5.558659            47
# 13: 2016   0.007     -0.1   1.1    154          85       14      5.883117            46
# 14: 2017   0.002     -0.3   0.3    164         109       11      6.000000            42
# 15: 2018   0.008     -0.4   1.2    143          86       16      6.657343            39
# 16: 2019   0.011     -0.3   1.1     98          69       10      4.948980            39
# 17: 2020   0.013     -1.2   1.9    145          81       15      5.696552            41
# 18: 2021   0.007     -0.5   1.1    158          91       15      6.202532            46
# 19: 2022   0.015     -0.8   2.1    140          68       17      5.621429            37
prices[((low<running_low*1.001)|(avg_delta_short<avg_delta*.98)) &  
         cap_order<50 & 
         (((close-low)/avg_range)<.15 ) & 
         lead1sell_rally/lead1open<1.5][
           order(avg_delta_short,decreasing = F),head(.SD,3),date] %>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol,
                   lead1sell_rallydate))

#############
# earners
#############
# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.01292857 0.01126126     -0.8           231        1124        9
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2009   0.019      0.0   0.2     13          14        3      7.000000             1
# 2: 2010   0.020     -0.1   1.5     73          74        6      5.095890             8
# 3: 2011   0.006     -0.5   0.7    112         113        9      6.964286            12
# 4: 2012   0.004     -0.5   0.4    106         107        6      6.726415             9
# 5: 2013   0.013     -0.3   1.0     76          77        6      5.513158             6
# 6: 2014   0.008     -0.1   0.5     58          59        5      5.862069             5
# 7: 2015   0.022     -0.1   0.2      7           8        3      4.714286             3
# 8: 2016   0.021     -0.5   1.2     58          59        6      5.689655             4
# 9: 2017   0.016     -0.4   0.9     56          57        5      6.214286             5
# 10: 2018   0.014     -0.2   1.2     84          85        9      6.880952             6
# 11: 2019   0.009     -0.1   0.7     78          79        8      5.602564             9
# 12: 2020   0.010     -0.8   1.5    149         150        9      6.416107            25
# 13: 2021   0.020     -0.6   2.6    134         135        8      6.783582            19
# 14: 2022  -0.001     -0.6  -0.1    106         107        9      6.924528            19

prices[close>7 & avg_volume>250000 & 
         (mean_eps/close) %between% c(.15, 1000) &  eps_unit=="USD / shares" & 
         avg_delta>.99 &
         (((close-low)/avg_range)<.15 ) & 
         lead1sell_rally/lead1open<1.5][
           order(close/lag1close,decreasing = F),head(.SD,1),date] %>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol,
                   lead1sell_rallydate))





bigcaps = prices[volume>500000 & close>7 & cap_order<200 & vp_order>50]
bigcaps[,bigcap_avg_delta:=mean(avg_delta),date]
bigcaps[,bigcap_avg_delta_short:=mean(avg_delta_short),date]

#############
# bigcap_short
# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.01157895 0.01130487     -2.1           573        1682       15
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2004   0.020     -0.1   0.4     21          22        5      5.904762             8
# 2: 2005  -0.011     -1.3  -1.1     99         100       10      6.575758            17
# 3: 2006   0.019     -1.2   1.9     98          99        7      6.561224            21
# 4: 2007   0.012     -0.8   1.4    117         118       13     10.179487            29
# 5: 2008   0.016     -2.1   3.1    196         197       12      6.724490            56
# 6: 2009   0.019     -2.0   3.4    178         179       13      7.191011            49
# 7: 2010   0.002     -0.4   0.2     76          77        8      7.921053            23
# 8: 2011   0.014     -0.3   1.0     69          70       15     11.202899            21
# 9: 2012   0.009     -0.1   0.5     51          52        8      8.862745            23
# 10: 2013   0.016     -0.1   0.8     54          55       10      9.092593            15
# 11: 2014   0.016     -0.4   0.8     52          53       10      8.134615             8
# 12: 2015   0.003     -0.6   0.2     69          70        9      7.724638            20
# 13: 2016   0.008     -0.5   0.7     86          87       11      8.872093            24
# 14: 2017   0.020     -0.1   0.4     18          19        5      7.222222            11
# 15: 2018   0.017     -0.2   0.6     33          34        4      8.484848            16
# 16: 2019   0.002     -0.6   0.1     76          77       11      8.315789            20
# 17: 2020   0.012     -0.7   2.1    174         175       10      6.672414            57
# 18: 2021   0.001     -0.5   0.2    111         112       10      8.720721            34
# 19: 2022   0.025     -0.3   2.1     85          86        8      6.294118            29

bigcaps[(avg_delta>1.0075 | avg_delta>bigcap_avg_delta*1.0075) & 
          (avg_delta_short>1.015 | avg_delta_short>bigcap_avg_delta_short*1.015) &
          lead1sell_lowclose/lead1open>.5][
            order(avg_delta_short,decreasing = T),head(.SD,1),date] %>%
  with(performance(date,1-lead1sell_lowclose/lead1open,lead1sell_lowclosedate-date,symbol,lead1sell_lowclosedate))


#############
# bigcap_long
# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.009421053 0.00887218     -1.3           225        2014       11
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2004   0.026      0.0   0.3     13          14        4      4.461538             9
# 2: 2005   0.012     -0.2   0.8     67          68        6      4.641791            21
# 3: 2006   0.012     -0.4   1.0     80          81        8      5.087500            37
# 4: 2007   0.001     -0.6   0.1    105         106        9      4.819048            34
# 5: 2008   0.015     -1.3   2.8    195         196        8      5.246154            75
# 6: 2009   0.009     -0.7   1.5    172         173       10      5.244186            74
# 7: 2010   0.006     -0.2   0.5     75          76        9      5.040000            35
# 8: 2011   0.009     -0.5   1.0    112         113        7      4.357143            38
# 9: 2012   0.005     -0.3   0.4     70          71        8      6.528571            26
# 10: 2013   0.005     -0.3   0.5     95          96        8      4.705263            32
# 11: 2014   0.021     -0.2   1.7     79          80        9      4.746835            25
# 12: 2015   0.007     -0.3   0.6     93          94        7      5.569892            32
# 13: 2016   0.010     -0.3   1.0     98          99        7      5.357143            33
# 14: 2017   0.008     -0.1   0.6     78          79        5      4.410256            24
# 15: 2018   0.005     -0.3   0.5     99         100        9      5.858586            35
# 16: 2019   0.008     -0.4   1.1    126         127       11      5.198413            37
# 17: 2020   0.012     -1.0   2.2    180         181       10      4.927778            60
# 18: 2021   0.005     -0.9   0.7    136         137       10      5.389706            51
# 19: 2022   0.003     -1.1   0.4    122         123       11      6.098361            48

bigcaps[(avg_delta_short<bigcap_avg_delta_short*.98 | ((MACD_slow - MACD) > .05)) &
          (avg_delta>bigcap_avg_delta*.995 | avg_delta>.995) &
          lead1sell_rally/lead1open<1.5][
            order(avg_delta_short,decreasing = F),head(.SD,1),date] %>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol,
                   lead1sell_rallydate))

###########
# No working strategies here yet
wins_by_hour = function(trade_data){ #Needs date, ticker, open and delta
  pennyshort_hours = get_hours_for_stocks(trade_data$symbol,
                                          start_date=min(trade_data$date),
                                          end_date=Sys.Date(),
                                          key=POLYKEY)
  res = merge(trade_data, pennyshort_hours,
              by.x=c('symbol','date'),by.y=c('stock','bar_date'))
  print(res[ !is.na(AdjClose_9),.(mean(open/Open_9,na.rm=T),
                                  at10=mean(AdjClose_9/open,na.rm=T),
                                  mean(AdjClose_10/open,na.rm=T),
                                  mean(AdjClose_11/open,na.rm=T),
                                  mean(AdjClose_12/open,na.rm=T),
                                  mean(AdjClose_13/open,na.rm=T),
                                  mean(AdjClose_14/open,na.rm=T),
                                  at359 = mean(AdjClose_15/open,na.rm=T),
                                  atclose = mean(Open_16/open,na.rm=T),
                                  delta = mean(close/open,na.rm=T),.N)])
  res
}






DEMA = function(x,period){
  2*EMA(x, n=period, align='right', fill=NA)-
    EMA(EMA(x, n=period, align='right', fill=NA), n=period, align='right', fill=NA)
}

prices[(symbol %in% prices[!is.na((open)/2),.N,symbol][N>50,symbol]),
       DEMA_s:=DEMA((open)/2 ,period = 10),
       symbol ]
prices[,lag1dema_s:= shift(DEMA_s,1,type='lag'),symbol]
prices[,lag2dema_s:= shift(DEMA_s,2,type='lag'),symbol]

prices[(symbol %in% prices[!is.na(open),.N,symbol][N>650,symbol]),
       DEMA_l:=SMA(open ,n = 50, align='right', fill=NA),
       symbol ]
prices[,lag1dema_l:= shift(DEMA_l,1,type='lag'),symbol]
prices[,lag2dema_l:= shift(DEMA_l,2,type='lag'),symbol]
prices[,lead30close:= shift(close,30,type='lead'),symbol]

prices[,avg_delta_trend:=NULL]
prices[symbol %in% prices[,.N,symbol][N>1000,symbol],
       avg_delta_trend:= SMA(ifelse(DEMA_s>DEMA_l, close/lag1close-1, 1-close/lag1close), n = 25 ),symbol ]

prices[,daily_delta_trend:=NULL]
prices[avg_volume>500000 & close>7,
       daily_delta_trend:=median(avg_delta,na.rm=T),date]

#prices[,lag1avgdelta:= shift(avg_delta,1,type='lag'),symbol]

prices[avg_volume>500000 & close>7 & (avg_delta_short<.95|avg_delta<.96) &
         days_around>100 &
         close<lag1high & sell_rally_day>5 ][
           order(avg_delta_short,decreasing = F),head(.SD,1),date] %>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol))

prices[volume>500000 & close>7 & (avg_delta5<.95|avg_delta25<.96) 
       & days_around>100 &
         close<lag1high & sell_rally_day>5 ][
           order(avg_delta5,decreasing = F),head(.SD,1),date] %>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol))
# same as correlated long?


rally(prices,
      sell_rule=function(dat){(dat[,DEMA_s<lag1dema_s & lag1dema_s>lag2dema_s] ) },
      varname='sell_trend')
prices[,lead1sell_trend:= shift(sell_trend,1,type='lead'),symbol]
prices[,lead1sell_trenddate:= shift(sell_trend_date,1,type='lead'),symbol]

prices[,avg_sl_ratio:=NULL]
prices[avg_volume>500000 & close>7,avg_sl_ratio:=median(DEMA_s/DEMA_l,na.rm=T),date]

setorder(prices, symbol, date)
prices[,purchase_i:=NULL]
prices[ (DEMA_s/DEMA_l-avg_sl_ratio) < -.35 & DEMA_s>lag1dema_s  & lag1dema_s<lag2dema_s & days_around>350, 
        purchase_i := rowid(symbol)]
x=prices[avg_volume>1000000 & close>7 & #lead1sell_trend/lead1open<2 &
           purchase_i==1 & year(date)==2018 & days_around>600,
         .(date,symbol,DEMA_s,DEMA_l,avg_sl_ratio)][sample(.N, 1)]
prices[symbol==x$symbol & year(date) %in% c(2016,2017,2018),.(date,close)] %>%plot
prices[symbol==x$symbol,.(date,DEMA_s)] %>%points(type='l',col='yellow')
prices[symbol==x$symbol,.(date,DEMA_l)] %>%points(type='l',col='blue')
abline(v=as.Date(x$date))

setorder(prices, symbol, date)
prices[avg_volume>500000 & close>7 & #lead1sell_trend/lead1open<2 &
         purchase_i==1 & days_around>350][
           order(-DEMA_s/DEMA_l),head(.SD,5), date ]%>%
  with(performance(date,lead30close/lead1open-1,lead1sell_trenddate-date,symbol))


# stock splits
all_splits= prices$symbol %>% unique %>%
  parallel::mclapply(
    stock_splits,
    key=POLYKEY,
    mc.cores = 16) %>% 
  rbindlist(use.names=TRUE, fill=T)

merge(prices,
      all_splits[,.(date=as.Date(execution_date),split_from,split_to,symbol=ticker)])



