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
prices=fread("~/datasets/stock_prices_15y.csv")
prices = prices[!is.na(volume) & !is.na(close) & !is.na(open)]

# prices=get_financials(prices,id_type='symbol')
setorder(prices, symbol, date)
lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)
performance_features(prices)

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
# prices[
#   close/open>1.025 & spy_future_night_delta>.99 & 
#     volume%between%c(10000,20000) & close>5 & lead1open/close<.975]%>%
#   with(performance(date,lead1close/lead1open-1,1,symbol))

# prices[
#   volume/avg_volume <.75 & spy_future_night_delta>.99 & 
#     volume%between%c(10000,20000) & close>5 & lead1open/close<.975]%>%
#   with(performance(date,lead1close/lead1open-1,1,symbol))

# prices[
#   (volume/avg_volume <.75 | close/open>1.025) & spy_future_night_delta>.99 & 
#     volume%between%c(10000,20000) & close>5 & (lead1open/close)<.975]%>%
#   with(performance(date,(lead1close/lead1open)-1,1,symbol))

#At open, buy stocks that climbed yesterday but fell overnight today unless the index fell overnight
#####

#####overbought
# prices[
#   (close/open)>1.2 & close>7 & (open/lag1close)>1  &
#     vp_order<3000][order(day_rise_norm,decreasing=T),.SD[1],date]%>% 
#   with(performance(date,1-lead1sell_lowclose/lead1open,1,symbol,lead1sell_lowclosedate,hold_less_than=5))


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
# 1:   0.0125 0.01262255     -0.9          1155         834        5
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2005   0.016     -0.1   0.8     49          50        4             1            47
# 2: 2006   0.011     -0.3   0.5     46          47        4             1            36
# 3: 2007   0.026     -0.1   1.4     54          55        5             1            47
# 4: 2008   0.022     -0.7   2.0     92          93        5             1            86
# 5: 2009  -0.004     -0.9  -0.2     52          53        4             1            46
# 6: 2010   0.023     -0.8   0.9     39          40        5             1            38
# 7: 2011   0.004     -0.4   0.2     56          57        5             1            49
# 8: 2012   0.014     -0.1   0.6     43          44        5             1            40
# 9: 2013   0.012     -0.2   0.6     50          51        4             1            46
# 10: 2014   0.011     -0.1   0.6     52          53        5             1            47
# 11: 2015   0.020     -0.3   1.0     47          48        5             1            43
# 12: 2016   0.014     -0.1   0.7     49          50        4             1            47
# 13: 2017   0.004     -0.1   0.1     19          20        3             1            17
# 14: 2018  -0.008     -0.5  -0.4     49          50        5             1            44
# 15: 2019   0.021     -0.5   0.4     19          20        2             1            18
# 16: 2020   0.000     -0.8   0.0     32          33        5             1            31
# 17: 2021  -0.005     -0.7  -0.2     38          39        5             1            37
# 18: 2022   0.044     -0.4   1.3     30          31        4             1            27

prices[lead1sell_rally/lead1open<1.5 & close>7 & volume>100000 & #exclude stuff that can't be traded
         volume>=max_volume & 
         avg_delta_short<.99 & 
         vp_order>cap_order &
         (close-low)/avg_range<.1][ #stock is boring
           order(day_drop_norm,decreasing = F),head(.SD,1),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol,
                   lead1sell_rallydate,hold_less_than = 5))



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

# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.02277778 0.01733668     -2.9           889         814        5
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2005   0.003     -0.6   0.1     26          27        5      4.653846             7
# 2: 2006   0.023     -0.2   0.5     21          22        5      3.571429             8
# 3: 2007   0.021     -2.6   1.1     50          51        5      6.260000            15
# 4: 2008   0.005     -2.9   0.5    106         107        5      6.009434            59
# 5: 2009   0.046     -2.2   1.9     41          42        5      5.439024            12
# 6: 2010   0.031     -0.1   0.7     21          22        5      3.095238             5
# 7: 2011   0.029     -0.2   0.9     31          32        5      5.806452            12
# 8: 2012   0.053     -0.1   1.2     23          24        5      5.434783             7
# 9: 2013   0.087      0.0   0.7      8           9        3      5.250000             4
# 10: 2014   0.035     -0.4   1.4     40          41        5      5.200000            12
# 11: 2015   0.008     -1.5   0.4     50          51        5      5.920000            18
# 12: 2016   0.017     -1.0   0.6     38          39        5     10.184211            11
# 13: 2017  -0.003     -1.1   0.0     15          16        5      9.400000             3
# 14: 2018   0.010     -1.0   0.4     44          45        5      8.045455            13
# 15: 2019   0.035     -0.7   1.5     42          43        5     10.166667            15
# 16: 2020   0.040     -1.9   3.0     74          75        5      4.554054            40
# 17: 2021   0.004     -0.9   0.5    119         120        5      6.126050            50
# 18: 2022  -0.034     -2.4  -1.6     47          48        5      7.510638            18


prices[close>7 & avg_volume>1000000 & 
         close<lag1high & sell_rally_day>4 & 
         avg_delta<.975][
         ][order(day_drop_norm, decreasing=F),head(.SD,1),date] %>%
  with(performance(date,
                   lead1sell_rally/lead1open-1,
                   lead1sell_rallydate-date,symbol,
                   lead1sell_rallydate, hold_less_than = 5))

# prices[close>7 & avg_volume>500000 & 
#          close>lag1high & sell_rally_day<2 & 
#          avg_delta_short>1.1][
#          ][order(day_rise_norm, decreasing=T),head(.SD,1),date]%>%
#   with(performance(date,
#                    1-lead1sell_lowclose/lead1open,
#                    lead1sell_lowclosedate-date,symbol,
#                    lead1sell_lowclosedate,
#                    hold_less_than = 5))


############
# nightbot
# 
# prices[close>5 & volume>100000 & lead1open/close>1.15 & spy_future_night_delta<1.005 ]%>%
#   with(performance(date,1-lead1sell_lowclose/lead1open,
#                    1,symbol))

##############
# megacap
# avg_year avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.01152632  0.012313       -1           364         888        5
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2004   0.013      0.0   0.1     10          11        5      5.200000             3
# 2: 2005   0.010      0.0   0.2     23          24        5      6.739130             4
# 3: 2006   0.010     -0.3   0.3     33          34        5      3.575758             7
# 4: 2007   0.007     -0.3   0.4     54          55        5      4.611111            12
# 5: 2008   0.015     -0.5   1.9    126         127        5      4.880952            22
# 6: 2009   0.028     -1.0   2.9    104         105        5      4.759615            21
# 7: 2010   0.003     -0.4   0.2     69          70        5      5.159420            16
# 8: 2011  -0.001     -0.4   0.0     49          50        5      4.734694            16
# 9: 2012   0.014     -0.3   0.4     29          30        3      3.275862            10
# 10: 2013   0.017      0.0   0.3     19          20        3      2.473684             6
# 11: 2014   0.017      0.0   0.3     16          17        5      4.812500             6
# 12: 2015   0.007     -0.2   0.2     30          31        5      5.666667            10
# 13: 2016   0.017     -0.2   0.2     12          13        4      3.666667             7
# 14: 2017   0.004     -0.1   0.0      9          10        4      3.666667             5
# 15: 2018   0.007     -0.3   0.4     60          61        5      6.433333            15
# 16: 2019   0.015     -0.2   0.5     35          36        5      5.200000             8
# 17: 2020   0.019     -0.2   1.2     62          63        5      5.225806            23
# 18: 2021   0.002     -0.3   0.1     57          58        5      5.736842            18
# 19: 2022   0.015     -0.2   1.1     72          73        5      4.597222            20


prices[avg_delta_short<avg_delta*.985 &  
         cap_order<25 & 
         lead1sell_rally/lead1open<1.5][
           order(day_drop_norm, decreasing=T),head(.SD,1),date] %>%
           #order(day_drop_norm, decreasing=F),head(.SD,1),date] %>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol,
                   lead1sell_rallydate,hold_less_than = 5))

#############
# earners
#############
# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.02171429 0.01861472     -0.8           411         707        5
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2009   0.023      0.0   0.1      5           6        2             1             1
# 2: 2010   0.048     -0.1   0.6     13          14        5             1             1
# 3: 2011   0.032     -0.5   1.3     40          41        5             1             5
# 4: 2012   0.026      0.0   0.8     30          31        5             1             3
# 5: 2013   0.021      0.0   0.6     27          28        5             1             2
# 6: 2014   0.005     -0.2   0.4     72          73        5             1             6
# 7: 2015  -0.001     -0.5  -0.1     49          50        5             1             4
# 8: 2016   0.011     -0.5   0.6     50          51        5             1             5
# 9: 2017   0.039     -0.1   1.6     41          42        5             1             2
# 10: 2018   0.029     -0.3   0.9     31          32        4             1             3
# 11: 2019   0.012     -0.1   0.6     48          49        5             1             6
# 12: 2020   0.019     -0.8   2.4    122         123        5             1            12
# 13: 2021   0.014     -0.7   1.3     93          94        5             1             9
# 14: 2022   0.026     -0.3   1.8     72          73        5             1            10

prices[lead1sell_rally/lead1open<1.5 & close>7 & avg_volume>250000 & 
    ( ((MACD_slow - MACD) > .1) | (low<running_low*1.001) | 
        (avg_delta_short<avg_delta*.98) | (sell_rally_day>10)) & 
      (mid_eps/close) >.2 &  eps_unit=="USD / shares"  ][ 
      order(mid_eps/close, decreasing=T),head(.SD,1),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol,
+                    lead1sell_rallydate,hold_less_than = 5))


prices[close>7 & avg_volume>250000 & #is.na(in_split_range) &
             avg_delta<1 & avg_delta_short<.995 &
            (mean_eps/close) >.05 &  eps_unit=="USD / shares"  ][
                order(avg_delta, decreasing=F),head(.SD,1),date][,.(lead1open[1],lead300close[1]/lead1open[1],date[1]),.(year(date),symbol)][order(year)][,.(mean(V2,na.rm=T),.N)]

bigcaps = prices[volume>500000 & close>7 & cap_order<200 & vp_order>50]
bigcaps[,bigcap_avg_delta:=mean(avg_delta,na.rm=T),date]
bigcaps[,bigcap_avg_delta_short:=mean(avg_delta_short,na.rm=T),date]

#############
# bigcap_short
                                          
# bigcaps[(avg_delta>1.0075 | avg_delta>bigcap_avg_delta*1.0075) & 
#           (avg_delta_short>1.015 | avg_delta_short>bigcap_avg_delta_short*1.015) &
#           lead1sell_lowclose/lead1open>.5][
#             order(day_rise_norm,decreasing=T),head(.SD,1),date] %>%
#   with(performance(date,1-lead1sell_lowclose/lead1open,lead1sell_lowclosedate-date,symbol,lead1sell_lowclosedate,hold_less_than = 5))


#############
# bigcap_long
# avg_year   avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.009842105 0.009360519     -1.4           484        1098        5
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2004   0.031      0.0   0.3     11          12        3      3.272727             6
# 2: 2005   0.019     -0.1   0.7     35          36        3      3.285714            17
# 3: 2006   0.000     -0.3   0.0     42          43        5      5.833333            25
# 4: 2007   0.022     -0.1   1.2     54          55        5      4.833333            32
# 5: 2008   0.015     -0.6   2.1    147         148        5      4.761905            68
# 6: 2009   0.003     -1.4   0.3     97          98        5      4.948454            58
# 7: 2010   0.002     -0.3   0.1     44          45        5      5.409091            27
# 8: 2011   0.011     -0.5   0.7     58          59        5      5.396552            34
# 9: 2012   0.005     -0.1   0.2     36          37        4      5.111111            21
# 10: 2013  -0.006     -0.2  -0.1     24          25        4      6.958333            15
# 11: 2014   0.015     -0.3   0.6     41          42        5      5.658537            17
# 12: 2015   0.000     -0.3   0.0     52          53        3      5.057692            26
# 13: 2016   0.010     -0.7   0.6     60          61        5      4.550000            32
# 14: 2017   0.017      0.0   0.3     19          20        3      4.052632            14
# 15: 2018   0.005     -0.1   0.2     48          49        5      5.708333            26
# 16: 2019   0.008     -0.5   0.5     66          67        5      5.378788            30
# 17: 2020   0.011     -0.5   1.1     98          99        5      4.500000            59
# 18: 2021   0.011     -0.3   0.7     65          66        5      4.738462            35
# 19: 2022   0.008     -0.5   0.6     82          83        5      5.439024            40

bigcaps[((avg_delta>.995 & avg_delta_short<.975) | (close>open*1.04 & avg_delta_short<1)) &
          lead1sell_rally/lead1open<1.5][
            order(volume, decreasing=F),head(.SD,1),date] %>%
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



