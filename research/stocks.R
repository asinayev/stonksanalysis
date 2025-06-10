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

# avg_year   avg_trade drawdown drawdown_days days_traded max_held
# 1:   0.0105 0.009433962       -1           789         972        5
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2005   0.004     -0.4   0.1     41          42        3      5.341463            39
# 2: 2006   0.012     -0.1   0.4     37          38        4      6.702703            33
# 3: 2007   0.010     -0.4   0.5     49          50        4      6.040816            46
# 4: 2008   0.006     -1.0   0.5     83          84        5      5.891566            76
# 5: 2009   0.031     -0.4   1.0     33          34        3      4.696970            33
# 6: 2010   0.017     -0.1   0.7     42          43        3      3.523810            42
# 7: 2011  -0.002     -0.5  -0.1     53          54        4      4.905660            50
# 8: 2012   0.003     -0.5   0.1     41          42        4      5.902439            39
# 9: 2013   0.020     -0.3   1.0     50          51        3      3.980000            50
# 10: 2014   0.008     -0.3   0.4     58          59        5      6.327586            53
# 11: 2015   0.013     -0.4   0.7     58          59        4      3.948276            57
# 12: 2016   0.026     -0.3   1.2     46          47        4      4.739130            45
# 13: 2017  -0.003     -0.4  -0.1     39          40        3      7.358974            36
# 14: 2018  -0.005     -0.9  -0.3     73          74        5      7.178082            70
# 15: 2019   0.013     -0.7   0.8     61          62        4      5.786885            61
# 16: 2020   0.009     -0.9   0.7     77          78        5      5.064935            75
# 17: 2021   0.007     -1.0   0.5     67          68        3      5.671642            66
# 18: 2022   0.020     -0.5   0.9     46          47        4      4.630435            45


prices[lead1sell_rally/lead1open<1.5 & close>7 & volume>100000 & #exclude stuff that can't be traded
         volume>=max_volume & 
         avg_delta_short<.99 & 
         vp_order>cap_order &
         (close-low)/avg_range<.1][ #stock is boring
           order(day_drop_norm,decreasing = F),head(.SD,1),date]%>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,
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
# 1: 0.02911111 0.01886792     -3.9           886         758        5
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2005   0.001     -0.6   0.0     23          24        4      3.739130             6
# 2: 2006   0.027     -0.3   0.4     16          17        3      1.500000             7
# 3: 2007  -0.002     -3.9  -0.1     48          48        5      4.437500            14
# 4: 2008   0.007     -2.9   0.8    110         111        5      4.836364            61
# 5: 2009   0.053     -1.9   2.1     39          40        5      3.666667            13
# 6: 2010   0.027     -0.1   0.6     21          22        4      1.904762             4
# 7: 2011   0.042     -0.1   1.1     26          27        5      2.615385            11
# 8: 2012   0.080     -0.1   1.1     14          15        5      5.785714             4
# 9: 2013   0.118      0.0   0.6      5           6        2      3.800000             3
# 10: 2014   0.055     -0.3   1.7     32          33        4      2.781250            10
# 11: 2015   0.015     -1.4   0.7     47          48        5      4.021277            17
# 12: 2016   0.017     -1.2   0.7     38          39        5      8.947368            11
# 13: 2017   0.024     -1.2   0.3     11          11        4      2.000000             2
# 14: 2018   0.014     -1.0   0.5     36          37        5      6.833333            11
# 15: 2019   0.030     -0.7   1.2     39          40        5      5.589744            15
# 16: 2020   0.043     -1.5   3.0     69          70        5      2.724638            38
# 17: 2021   0.010     -0.9   1.2    117         118        5      4.529915            48
# 18: 2022  -0.037     -2.5  -1.9     51          52        5      6.117647            20



prices[close>7 & avg_volume>1000000 & 
         close<lag1high & sell_rally_day>4 & 
         avg_delta<.975][
         ][order(day_drop_norm, decreasing=F),head(.SD,1),date] %>%
  with(performance(lead1date,
                   lead1sell_rally/lead1open-1,
                   lead1sell_rallydate-lead1date,symbol,
                   lead1sell_rallydate, hold_less_than = 5))

# prices[close>7 & avg_volume>500000 & 
#          close>lag1high & sell_rally_day<2 & 
#          avg_delta_short>1.1][
#          ][order(day_rise_norm, decreasing=T),head(.SD,1),date]%>%
#   with(performance(lead1date,
#                    1-lead1sell_lowclose/lead1open,
#                    lead1sell_lowclosedate-lead1date,symbol,
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
# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.01173684 0.01223582       -1           304         917        5
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2004   0.013      0.0   0.1     10          11        5      3.400000             3
# 2: 2005   0.010      0.0   0.2     23          24        5      5.391304             4
# 3: 2006   0.010     -0.3   0.3     34          35        5      2.264706             7
# 4: 2007   0.008     -0.3   0.4     56          57        5      3.321429            12
# 5: 2008   0.015     -0.4   2.0    135         136        5      3.414815            24
# 6: 2009   0.028     -1.0   3.1    108         108        5      3.342593            21
# 7: 2010   0.003     -0.4   0.2     72          73        5      4.055556            17
# 8: 2011   0.000     -0.4   0.0     50          51        5      3.180000            16
# 9: 2012   0.019     -0.3   0.4     24          25        3      1.708333            10
# 10: 2013   0.017      0.0   0.3     19          20        3      1.105263             6
# 11: 2014   0.017      0.0   0.3     16          17        4      3.312500             6
# 12: 2015   0.008     -0.2   0.2     31          32        5      4.161290            11
# 13: 2016   0.017     -0.2   0.2     12          13        3      2.500000             7
# 14: 2017   0.004     -0.1   0.0      9          10        3      2.000000             5
# 15: 2018   0.006     -0.3   0.4     64          65        5      4.953125            15
# 16: 2019   0.014     -0.3   0.5     37          38        5      3.621622             8
# 17: 2020   0.020     -0.2   1.3     66          67        5      3.590909            25
# 18: 2021   0.001     -0.4   0.1     59          60        5      4.559322            18
# 19: 2022   0.013     -0.3   1.0     74          75        5      3.202703            20

prices[avg_delta_short<avg_delta*.985 &  
         cap_order<25 & 
         lead1sell_rally/lead1open<1.5][
           order(day_drop_norm, decreasing=T),head(.SD,1),date] %>%
           #order(day_drop_norm, decreasing=F),head(.SD,1),date] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,
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
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,
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
# 1: 0.009421053 0.009090909     -1.4           483        1130        5
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2004   0.031      0.0   0.3     11          12        2      2.090909             6
# 2: 2005   0.019     -0.1   0.7     35          36        2      1.942857            17
# 3: 2006   0.001     -0.3   0.1     43          44        5      4.395349            25
# 4: 2007   0.021     -0.1   1.2     58          59        5      3.275862            34
# 5: 2008   0.014     -0.7   2.1    154         155        5      3.266234            70
# 6: 2009   0.002     -1.4   0.2    100         101        5      3.620000            58
# 7: 2010  -0.001     -0.3   0.0     46          47        5      4.130435            28
# 8: 2011   0.011     -0.5   0.7     60          61        5      3.883333            35
# 9: 2012   0.005     -0.1   0.2     35          36        4      3.771429            20
# 10: 2013  -0.005     -0.2  -0.1     25          26        4      5.440000            16
# 11: 2014   0.015     -0.3   0.6     41          42        4      4.365854            17
# 12: 2015   0.000     -0.3   0.0     53          54        3      3.849057            26
# 13: 2016   0.011     -0.7   0.7     62          63        5      2.967742            32
# 14: 2017   0.017      0.0   0.3     19          20        2      2.421053            14
# 15: 2018   0.005     -0.1   0.3     49          50        5      4.265306            26
# 16: 2019   0.007     -0.5   0.5     67          68        5      4.014925            30
# 17: 2020   0.013     -0.5   1.3    100         101        5      2.910000            59
# 18: 2021   0.010     -0.3   0.7     67          68        4      3.223881            36
# 19: 2022   0.003     -0.5   0.3     86          87        5      4.220930            41


bigcaps[((avg_delta>.995 & avg_delta_short<.975) | (close>open*1.04 & avg_delta_short<1)) &
          lead1sell_rally/lead1open<1.5][
            order(volume, decreasing=F),head(.SD,1),date] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,
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



