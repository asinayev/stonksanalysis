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
prices=lapply(2004:2018,
              function(yr){
                fread(paste0("/home/rstudio/datasets/stocks_by_yr/",yr,".csv.gz")) %>%
                  subset(type %in% c('CS','PF',''))
              })%>%
  rbindlist(use.names=T, fill=T)

prices = prices[!is.na(volume) & !is.na(close) & !is.na(open)]
# prices=get_financials(prices,id_type='symbol')
setorder(prices, symbol, date)
lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)
performance_features(prices)

prices[(symbol_session %in% prices[!is.na(close),.N,symbol_session][N>50,symbol_session]) & !is.na(close),
          dema_delta:=DEMA(close/lag1close,   n = 25),symbol_session ]

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
#      avg_year   avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.01088889 0.009758657     -1.2           804         971        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2005   0.004     -0.4   0.2     41          42        3      5.390244            38
#  2: 2006   0.011     -0.1   0.4     37          38        4      6.189189            33
#  3: 2007   0.016     -0.4   0.8     49          50        4      6.122449            47
#  4: 2008   0.008     -1.1   0.7     83          84        5      5.891566            72
#  5: 2009   0.038     -0.4   1.2     33          34        3      4.424242            33
#  6: 2010   0.017     -0.1   0.7     42          43        3      3.666667            42
#  7: 2011  -0.007     -0.8  -0.4     52          53        5      5.096154            49
#  8: 2012   0.003     -0.8   0.1     41          42        4      6.121951            39
#  9: 2013   0.021     -0.5   1.0     50          51        3      4.080000            49
# 10: 2014   0.008     -0.3   0.4     58          59        5      6.293103            53
# 11: 2015   0.011     -0.5   0.6     58          59        4      4.137931            57
# 12: 2016   0.023     -0.5   1.1     46          47        4      4.913043            45
# 13: 2017  -0.003     -0.4  -0.1     39          40        3      7.358974            36
# 14: 2018   0.003     -0.6   0.2     75          76        5      7.026667            72
# 15: 2019   0.015     -0.4   0.9     61          62        4      5.721311            61
# 16: 2020   0.002     -1.2   0.1     75          76        5      5.146667            73
# 17: 2021   0.011     -0.9   0.7     67          68        4      5.731343            65
# 18: 2022   0.015     -0.5   0.7     46          47        4      4.891304            45


prices[lead1sell_rally/lead1open<1.5 & close>7 & volume>100000 & #exclude stuff that can't be traded
         volume>=max_volume &
         (close-low)/avg_range<.1 & 
         avg_delta_short<.99 & 
         vp_order>cap_order ][
           order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,
                   symbol,lead1sell_rallydate,hold_max = 5,buy_per_day_max = 1, hold_same_max = F))



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

#    avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1:    0.034 0.02469799     -2.9           886         762        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2005   0.001     -0.6   0.0     23          24        4      3.739130             6
#  2: 2006   0.027     -0.3   0.4     16          17        3      1.500000             7
#  3: 2007   0.051     -2.0   2.4     48          49        5      4.166667            14
#  4: 2008   0.016     -2.9   1.8    113         114        5      4.539823            62
#  5: 2009   0.061     -1.0   2.4     39          40        5      3.666667            12
#  6: 2010   0.027     -0.1   0.6     21          22        4      1.904762             4
#  7: 2011   0.042     -0.1   1.1     26          27        5      2.615385            11
#  8: 2012   0.080     -0.1   1.1     14          15        5      5.785714             4
#  9: 2013   0.118      0.0   0.6      5           6        2      3.800000             3
# 10: 2014   0.076     -0.3   2.4     32          33        4      2.843750            11
# 11: 2015   0.012     -1.4   0.6     47          48        5      3.957447            16
# 12: 2016   0.017     -1.2   0.7     38          39        5      8.947368            11
# 13: 2017   0.024     -1.2   0.3     11          11        4      2.000000             2
# 14: 2018   0.014     -1.0   0.5     37          38        5      6.675676            11
# 15: 2019   0.030     -0.7   1.2     39          40        5      5.589744            15
# 16: 2020   0.036     -2.1   2.5     69          70        5      2.869565            38
# 17: 2021   0.012     -0.9   1.4    117         118        5      4.512821            47
# 18: 2022  -0.032     -2.4  -1.6     50          51        5      6.500000            19



prices[close>7 & cap_order<1500 &
         avg_volume>1000000 &
         close<lag1high & sell_rally_day>4 & 
         avg_delta<.98][
           order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,
                   symbol,lead1sell_rallydate,hold_max = 5,buy_per_day_max = 1, hold_same_max = F))

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
#     avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.01305263 0.01387347     -1.1           522         919        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2004   0.013      0.0   0.1     10          11        5      3.400000             3
#  2: 2005   0.010     -0.1   0.2     23          24        5      5.043478             5
#  3: 2006   0.014     -0.3   0.5     34          35        5      1.823529             5
#  4: 2007   0.008     -0.3   0.4     56          57        5      2.910714            10
#  5: 2008   0.020     -0.4   2.7    136         137        5      3.360294            21
#  6: 2009   0.029     -1.1   3.1    107         107        5      3.392523            17
#  7: 2010  -0.001     -0.6  -0.1     70          71        5      4.342857            14
#  8: 2011   0.013     -0.3   0.6     50          51        5      2.700000            13
#  9: 2012   0.018     -0.2   0.4     24          25        3      1.958333             9
# 10: 2013   0.016      0.0   0.3     19          20        3      1.105263             7
# 11: 2014   0.018      0.0   0.3     16          17        3      3.250000             5
# 12: 2015   0.010     -0.1   0.3     32          33        5      3.812500             8
# 13: 2016   0.016     -0.1   0.2     12          13        3      2.500000             6
# 14: 2017   0.007     -0.1   0.1      9          10        4      2.222222             4
# 15: 2018   0.008     -0.2   0.5     64          65        5      5.000000            13
# 16: 2019   0.013     -0.2   0.5     37          38        5      3.972973             7
# 17: 2020   0.027     -0.2   1.8     68          69        4      2.661765            19
# 18: 2021   0.009     -0.2   0.6     63          64        5      3.269841            15
# 19: 2022   0.000     -0.9   0.0     71          72        5      4.098592            13

prices[avg_delta_short<avg_delta*.985 &  
         cap_order<50 & lagging_corr_long>.7 & 
         lead1sell_rally/lead1open<1.5][
           order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,
                   symbol,lead1sell_rallydate,hold_max = 5,buy_per_day_max = 1, hold_same_max = F))


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
        order(date,close/mid_eps, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,
                   symbol,lead1sell_rallydate,hold_max = 5,buy_per_day_max = 1, hold_same_max = F))



prices[close>7 & avg_volume>250000 & #is.na(in_split_range) &
             avg_delta<1 & avg_delta_short<.995 &
            (mean_eps/close) >.05 &  eps_unit=="USD / shares"  ][
                order(avg_delta, decreasing=F),head(.SD,1),date][,.(lead1open[1],lead300close[1]/lead1open[1],date[1]),.(year(date),symbol)][order(year)][,.(mean(V2,na.rm=T),.N)]

bigcaps = prices[volume>500000 & close>7 & cap_order<250]
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
#       avg_year   avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.009526316 0.009442446     -1.3           607        1131        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2004   0.023      0.0   0.3     11          12        4      4.000000             6
#  2: 2005   0.019     -0.1   0.6     35          36        2      1.942857            16
#  3: 2006   0.008     -0.1   0.4     42          43        5      4.142857            25
#  4: 2007   0.007     -0.2   0.4     59          60        4      3.898305            35
#  5: 2008   0.016     -0.8   2.4    152         153        5      4.144737            67
#  6: 2009   0.014     -0.5   1.4    104         105        5      3.788462            53
#  7: 2010  -0.003     -0.3  -0.1     47          48        5      3.914894            32
#  8: 2011   0.009     -0.6   0.6     61          62        4      3.459016            36
#  9: 2012   0.006     -0.1   0.2     35          36        4      4.028571            22
# 10: 2013  -0.003     -0.2  -0.1     25          26        4      4.800000            16
# 11: 2014   0.020     -0.2   0.8     41          42        4      4.268293            16
# 12: 2015   0.005     -0.2   0.3     53          54        4      4.584906            27
# 13: 2016   0.013     -0.5   0.8     61          62        5      3.229508            21
# 14: 2017   0.017      0.0   0.3     19          20        2      2.421053            16
# 15: 2018   0.006     -0.1   0.3     48          49        5      4.770833            27
# 16: 2019   0.011     -0.3   0.8     68          69        5      3.882353            29
# 17: 2020   0.006     -1.3   0.6    100         101        5      3.420000            45
# 18: 2021   0.007     -0.6   0.5     67          68        4      3.000000            36
# 19: 2022   0.000     -0.6   0.0     84          85        5      5.047619            31



bigcaps[((avg_delta>.995 & avg_delta_short<.975) | 
           (close>open*1.05 & avg_delta_short<1)) &
          lead1sell_rally/lead1open<1.5][
            order(date,-volume, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,
                   symbol,lead1sell_rallydate,hold_max = 5,buy_per_day_max = 1, hold_same_max = F))



################
# night_rebound
################
# executed directly, not via R

prices[volume>500000 & close>7 & close/open<.825 & close<lag5close & 
         lead1sell_rally/lead1open<1.5][
           order(date,close/open, decreasing=F)] %>%
  with(performance(lead1date,lead1open/close-1,0,
                   symbol,lead1date,hold_max = 1,buy_per_day_max = F, hold_same_max = F))


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


