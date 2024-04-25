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
prices = prices[!is.na(volume) & !is.na(close) & !is.na(open)]

#prices=get_financials(prices,identifier='symbol')
setorder(prices, symbol, date)
lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)
prices[,lead1sell_rally:= shift(sell_rally,1,type='lead'),symbol]
prices[,lead1sell_rallydate:= shift(sell_rally_date,1,type='lead'),symbol]

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
# 1: 0.03489474 0.04293893     -3.3          1193        1067        5
#    year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2004   0.033      0.0   0.1      2           3        2             1             2
# 2: 2005   0.016      0.0   0.1      5           6        2             1             5
# 3: 2006  -0.026     -0.5  -0.3     13          14        2             1            12
# 4: 2007   0.068     -0.1   1.4     21          22        3             1            18
# 5: 2008   0.026     -0.8   2.0     74          75        4             1            64
# 6: 2009  -0.011     -2.5  -0.9     83          84        5             1            69
# 7: 2010   0.026     -2.1   0.8     32          33        4             1            29
# 8: 2011   0.014     -1.6   0.3     23          24        3             1            22
# 9: 2012   0.060     -1.1   1.7     29          30        3             1            21
# 10: 2013   0.038     -0.5   0.7     19          20        4             1            15
# 11: 2014   0.040     -0.8   1.1     27          28        5             1            21
# 12: 2015   0.026     -1.6   1.4     53          54        4             1            41
# 13: 2016  -0.024     -2.7  -1.4     58          59        5             1            49
# 14: 2017   0.081     -1.9   5.4     66          67        5             1            57
# 15: 2018   0.105     -1.4  10.5    100         101        5             1            80
# 16: 2019   0.024     -2.1   2.4     99         100        5             1            82
# 17: 2020   0.056     -2.7   8.2    145         146        5             1           121
# 18: 2021   0.060     -3.3   8.4    139         140        5             1           108
# 19: 2022   0.051     -1.1   3.1     60          61        5             1            51

prices[
  (close/open)>1.2 & close>7 & (open/lag1close)>1  &
    vp_order<3000][order(close*volume,decreasing=T),.SD[1],date]%>% 
  with(performance(date,1-lead1sell_lowclose/lead1open,1,symbol,lead1sell_lowclosedate,hold_less_than=5))


# At open, sell stocks that climbed yesterday too much
#####


###### volumeshort
# prices[lag1volume/volume_avg>7.5 & lag1_day_delta>.975 & night_delta>1.01  & open>7.5 &
#          lag1volume%between%c(10000,100000),
#        .(mean(day_delta,na.rm=T),.N), year(date)][order(year)]
#
######


#####volumelong -- new version

#      avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.01055556 0.01109741     -1.4           798         829        5
#    year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2005   0.013     -0.2   0.7     49          50        3             1            49
# 2: 2006   0.008     -0.4   0.4     46          47        4             1            40
# 3: 2007   0.026     -0.1   1.4     54          55        5             1            51
# 4: 2008   0.021     -0.7   1.9     91          92        5             1            86
# 5: 2009  -0.002     -0.9  -0.1     51          52        5             1            45
# 6: 2010   0.023     -0.6   0.9     39          40        5             1            38
# 7: 2011   0.001     -0.5   0.0     55          56        5             1            48
# 8: 2012   0.014     -0.3   0.6     43          44        5             1            40
# 9: 2013   0.016     -0.1   0.8     50          51        4             1            46
# 10: 2014   0.011     -0.2   0.6     53          54        5             1            48
# 11: 2015   0.019     -0.3   0.9     47          48        5             1            41
# 12: 2016   0.012     -0.1   0.6     49          50        3             1            48
# 13: 2017   0.003     -0.1   0.1     19          20        3             1            17
# 14: 2018  -0.008     -0.5  -0.4     49          50        5             1            42
# 15: 2019   0.021     -0.4   0.4     19          20        2             1            18
# 16: 2020  -0.018     -1.0  -0.6     31          32        5             1            30
# 17: 2021  -0.014     -1.4  -0.5     36          37        5             1            33
# 18: 2022   0.044     -1.2   1.3     30          31        4             1            28

prices[lead1sell_rally/lead1open<1.5 & close>5 & volume>100000 & #exclude stuff that can't be traded
         (volume>=max_volume & avg_delta_short<.99) & #big down movement recently and consolidated today
         (((close-low)/avg_range)<.2 ) & 
         (log(vp_order)-log(cap_order))>.4 ][ #stock is boring
           order(avg_delta_short),head(.SD,1),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,1,symbol,lead1sell_rallydate,hold_less_than = 5))



#####
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




###### Correlated long and short
rally_avg(prices,100)

#      avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.02822222 0.02069409     -4.5          1092         796        5
#    year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2005   0.008     -0.6   0.2     26          27        5      4.653846             6
# 2: 2006   0.130     -0.1   2.6     20          21        5      3.850000             9
# 3: 2007  -0.026     -4.5  -1.2     46          47        5      7.195652            16
# 4: 2008   0.000     -3.5   0.0    104         105        5      6.230769            50
# 5: 2009   0.048     -3.3   2.0     41          42        5      5.487805            15
# 6: 2010   0.033     -1.0   0.7     21          22        5      3.333333             5
# 7: 2011   0.023     -0.3   0.7     31          32        5      6.258065            11
# 8: 2012   0.058     -0.1   1.3     22          23        5      5.454545             7
# 9: 2013   0.087      0.0   0.7      8           9        3      5.250000             4
# 10: 2014   0.044     -0.5   1.8     41          42        5      5.146341            13
# 11: 2015   0.019     -1.0   0.9     49          50        5      6.142857            17
# 12: 2016   0.011     -1.1   0.4     37          38        5     10.594595            13
# 13: 2017  -0.003     -1.1   0.0     15          16        5      9.400000             3
# 14: 2018   0.016     -1.1   0.7     43          44        5      8.209302            14
# 15: 2019   0.032     -0.8   1.5     47          48        5      8.936170            16
# 16: 2020   0.061     -1.3   4.4     72          73        5      5.097222            38
# 17: 2021   0.009     -0.9   1.1    115         116        5      7.504348            52
# 18: 2022  -0.042     -3.0  -1.7     40          41        5      8.900000            18

prices[close>7 & avg_volume>1000000 & 
         close<lag1high & sell_rally_day>4 & 
         avg_delta<.975][
         ][order(high/close, decreasing=T),head(.SD,1),date] %>%
  with(performance(date,
                   lead1sell_rally/lead1open-1,
                   lead1sell_rallydate-date,symbol,
                   lead1sell_rallydate, hold_less_than = 5))

# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.03784211 0.04644628     -3.7          1414         624        5
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2004  -0.001      0.0   0.0      1           2        1     13.000000             1
# 2: 2005   0.025      0.0   0.1      3           4        2      8.000000             2
# 3: 2006   0.000     -0.1   0.0      7           8        2      5.857143             5
# 4: 2007  -0.007     -0.1  -0.1      9          10        3     11.333333             7
# 5: 2008   0.061     -0.4   1.9     31          32        4      6.000000            25
# 6: 2009   0.023     -1.2   0.8     33          34        4      6.818182            29
# 7: 2010   0.008     -1.0   0.1      9          10        2      6.333333             7
# 8: 2011  -0.003     -1.0   0.0     17          18        5     12.235294            11
# 9: 2012   0.031     -1.0   0.2      5           6        2      9.800000             4
# 10: 2013   0.230     -0.5   2.1      9          10        3     12.888889             6
# 11: 2014   0.043     -0.3   0.6     15          16        3      4.466667             8
# 12: 2015   0.066     -0.4   1.1     17          18        4      4.235294            12
# 13: 2016  -0.116     -3.4  -3.1     27          28        5      6.629630            14
# 14: 2017   0.065     -3.7   1.6     25          26        4      7.280000            18
# 15: 2018   0.126     -1.3   5.2     41          42        5      7.634146            31
# 16: 2019   0.040     -0.8   2.3     58          59        5      7.310345            45
# 17: 2020   0.079     -1.7  10.5    133         134        5      7.195489            97
# 18: 2021   0.036     -2.9   4.1    115         116        5      5.913043            89
# 19: 2022   0.013     -1.7   0.7     50          51        5      8.420000            34

prices[close>7 & avg_volume>500000 & 
         close>lag1high & sell_rally_day<2 & 
         avg_delta_short>1.1][
         ][order(days_around, decreasing=T),head(.SD,1),date]%>%
  with(performance(date,
                   1-lead1sell_lowclose/lead1open,
                   lead1sell_lowclosedate-date,symbol,
                   lead1sell_lowclosedate,
                   hold_less_than = 5))


############
# nightbot
# 
# prices[close>5 & volume>100000 & lead1open/close>1.15 & spy_future_night_delta<1.005 ]%>%
#   with(performance(date,1-lead1sell_lowclose/lead1open,
#                    1,symbol))

##############
# megacap
#      avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.02822222 0.02069409     -4.5          1092         796        5
#    year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2005   0.008     -0.6   0.2     26          27        5      4.653846             6
# 2: 2006   0.130     -0.1   2.6     20          21        5      3.850000             9
# 3: 2007  -0.026     -4.5  -1.2     46          47        5      7.195652            16
# 4: 2008   0.000     -3.5   0.0    104         105        5      6.230769            50
# 5: 2009   0.048     -3.3   2.0     41          42        5      5.487805            15
# 6: 2010   0.033     -1.0   0.7     21          22        5      3.333333             5
# 7: 2011   0.023     -0.3   0.7     31          32        5      6.258065            11
# 8: 2012   0.058     -0.1   1.3     22          23        5      5.454545             7
# 9: 2013   0.087      0.0   0.7      8           9        3      5.250000             4
# 10: 2014   0.044     -0.5   1.8     41          42        5      5.146341            13
# 11: 2015   0.019     -1.0   0.9     49          50        5      6.142857            17
# 12: 2016   0.011     -1.1   0.4     37          38        5     10.594595            13
# 13: 2017  -0.003     -1.1   0.0     15          16        5      9.400000             3
# 14: 2018   0.016     -1.1   0.7     43          44        5      8.209302            14
# 15: 2019   0.032     -0.8   1.5     47          48        5      8.936170            16
# 16: 2020   0.061     -1.3   4.4     72          73        5      5.097222            38
# 17: 2021   0.009     -0.9   1.1    115         116        5      7.504348            52
# 18: 2022  -0.042     -3.0  -1.7     40          41        5      8.900000            18
prices[((low<running_low*1.001)|(avg_delta_short<avg_delta*.98)) &  
         cap_order<50 & 
         (((close-low)/avg_range)<.15 ) & 
         lead1sell_rally/lead1open<1.5][
           order(avg_delta_short,decreasing = F),head(.SD,1),date] %>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol,
                   lead1sell_rallydate,hold_less_than = 5))

############
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

prices[lead1sell_rally/lead1open<1.5 & close>7 & avg_volume>250000 &
         ( ((MACD_slow - MACD) > .03) | (low<running_low*1.005) | 
             (avg_delta_short<avg_delta*.985) | (sell_rally_day>6)) & 
         (mean_eps/close) %between% c(.2, 100) &  eps_unit=="USD / shares"  ][ #stock is boring
           order(avg_delta_short),head(.SD,1),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,1,symbol,lead1sell_rallydate))



bigcaps = prices[volume>500000 & close>7 & cap_order<200 & vp_order>50]
bigcaps[,bigcap_avg_delta:=mean(avg_delta),date]
bigcaps[,bigcap_avg_delta_short:=mean(avg_delta_short),date]

#############
# bigcap_short
#    avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.01168421 0.01148177       -2           398        1308        5
#    year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2004   0.019     -0.1   0.4     20          21        5      6.100000             8
# 2: 2005  -0.011     -1.1  -0.9     81          82        5      6.419753            14
# 3: 2006   0.019     -1.0   1.7     92          93        5      6.239130            21
# 4: 2007   0.007     -0.8   0.5     74          75        5     12.027027            27
# 5: 2008   0.020     -1.9   3.0    154         155        5      6.409091            50
# 6: 2009   0.016     -2.0   1.9    114         115        5      8.000000            42
# 7: 2010   0.000     -0.3   0.0     64          65        5      7.937500            22
# 8: 2011   0.015     -0.2   0.8     53          54        5     10.018868            19
# 9: 2012   0.009     -0.1   0.4     44          45        5      9.113636            21
# 10: 2013   0.016     -0.1   0.7     47          48        5      9.404255            13
# 11: 2014   0.015     -0.4   0.6     41          42        5      8.365854             7
# 12: 2015   0.000     -0.6   0.0     55          56        5      7.872727            18
# 13: 2016   0.008     -0.5   0.6     70          71        5      8.457143            20
# 14: 2017   0.026     -0.1   0.4     16          17        5      6.937500             9
# 15: 2018   0.017     -0.1   0.6     33          34        4      8.484848            16
# 16: 2019   0.003     -0.4   0.2     54          55        5      7.925926            18
# 17: 2020   0.015     -0.5   1.9    127         128        5      7.330709            46
# 18: 2021   0.001     -0.4   0.0     77          78        5      9.441558            27
# 19: 2022   0.027     -0.2   2.0     73          74        5      6.328767            27

bigcaps[(avg_delta>1.0075 | avg_delta>bigcap_avg_delta*1.0075) & 
          (avg_delta_short>1.015 | avg_delta_short>bigcap_avg_delta_short*1.015) &
          lead1sell_lowclose/lead1open>.5][
            order(avg_delta_short,decreasing = T),head(.SD,1),date] %>%
  with(performance(date,1-lead1sell_lowclose/lead1open,lead1sell_lowclosedate-date,symbol,lead1sell_lowclosedate,hold_less_than = 5))


#############
# bigcap_long
#       avg_year   avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.008157895 0.007804132     -0.9           356        1326        5
#    year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2004   0.026      0.0   0.3     13          14        4      4.461538             9
# 2: 2005   0.013     -0.1   0.5     36          37        4      4.361111            18
# 3: 2006   0.014     -0.2   0.7     53          54        5      5.094340            28
# 4: 2007   0.004     -0.3   0.2     57          58        5      5.543860            28
# 5: 2008   0.015     -0.9   2.2    147         148        5      5.306122            64
# 6: 2009   0.007     -0.9   0.8    119         120        5      5.428571            61
# 7: 2010   0.001     -0.3   0.0     52          53        5      5.461538            29
# 8: 2011   0.005     -0.5   0.3     62          63        5      4.903226            28
# 9: 2012   0.004     -0.3   0.2     57          58        5      7.017544            24
# 10: 2013   0.005     -0.2   0.3     55          56        5      5.290909            26
# 11: 2014   0.018     -0.2   0.9     47          48        5      4.829787            22
# 12: 2015   0.007     -0.3   0.5     72          73        5      5.763889            31
# 13: 2016   0.006     -0.4   0.4     70          71        5      5.842857            26
# 14: 2017   0.005     -0.1   0.2     32          33        4      5.437500            19
# 15: 2018   0.002     -0.3   0.1     66          67        5      6.590909            30
# 16: 2019   0.003     -0.4   0.2     77          78        5      5.610390            31
# 17: 2020   0.019     -0.4   2.3    125         126        5      5.048000            54
# 18: 2021   0.000     -0.9   0.0     78          79        5      5.641026            33
# 19: 2022   0.001     -0.9   0.1     89          90        5      6.292135            41

bigcaps[(avg_delta_short<bigcap_avg_delta_short*.98 | ((MACD_slow - MACD) > .05)) &
          (avg_delta>bigcap_avg_delta*.995 | avg_delta>.995) &
          lead1sell_rally/lead1open<1.5][
            order(avg_delta_short,decreasing = F),head(.SD,1),date] %>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol,
                   lead1sell_rallydate,hold_less_than = 5))

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



# stock splits
all_splits= prices$symbol %>% unique %>%
  parallel::mclapply(
    stock_splits,
    key=POLYKEY,
    mc.cores = 16) %>% 
  rbindlist(use.names=TRUE, fill=T)

merge(prices,
      all_splits[,.(date=as.Date(execution_date),split_from,split_to,symbol=ticker)])



