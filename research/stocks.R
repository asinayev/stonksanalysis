require(tidyquant)
require(data.table)
require(rpart)
require(ggplot2)

setwd('~/stonksanalysis')
source("polygon.R", local=T)
source("implement/features.R", local=T)
POLYKEY = Sys.getenv('POLYGONKEY')

# wins_by_hour = function(trade_data){ #Needs date, ticker, open and delta
#   pennyshort_hours = get_hours_for_stocks(trade_data$ticker,
#                                           start_date=min(trade_data$date), 
#                                           end_date=Sys.Date(),
#                                           key=POLYKEY)
#   res = merge(trade_data, pennyshort_hours, 
#         by.x=c('ticker','date'),by.y=c('stock','bar_date'))
#   print(res[ !is.na(AdjClose_9),.(mean(open/Open_9,na.rm=T),
#                                   at10=mean(AdjClose_9/open,na.rm=T),
#                                   mean(AdjClose_10/open,na.rm=T),
#                                   mean(AdjClose_11/open,na.rm=T),
#                                   mean(AdjClose_12/open,na.rm=T),
#                                   mean(AdjClose_13/open,na.rm=T),
#                                   mean(AdjClose_14/open,na.rm=T),
#                                   at359 = mean(AdjClose_15/open,na.rm=T),
#                                   atclose = mean(Open_16/open,na.rm=T),
#                                   delta = mean(delta,na.rm=T),.N)])
#   res
# }
# 
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
prices[,days_around:=cumsum(!is.na(close)),symbol]

lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)
# rally_avg(prices,200)

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
    volume%between%c(10000,20000) & close>5 & future_night_delta<.975]%>%
    with(performance(date,future_day_delta-1,1,symbol))

prices[
  volume/volume_avg <.75 & spy_future_night_delta>.99 & 
    volume%between%c(10000,20000) & close>5 & future_night_delta<.975]%>%
  with(performance(date,future_day_delta-1,1,symbol))

prices[
  (volume/avg_volume <.75 | close/open>1.025) & spy_future_night_delta>.99 & 
    volume%between%c(10000,20000) & close>5 & (lead1open/close)<.975]%>%
  with(performance(date,(lead1close/lead1open)-1,1,symbol))

#At open, buy stocks that climbed yesterday but fell overnight today unless the index fell overnight
#####

#####overbought
# perf drawdown days_traded
# 1: 0.01910526       -2        1158
# year average drawdown total trades days_traded avg_days_held stocks_traded
# 1: 2004   0.028      0.0   0.1      2           2             1             2
# 2: 2005   0.018     -0.1   0.1      5           5             1             5
# 3: 2006   0.001     -0.2   0.0     11          11             1            10
# 4: 2007   0.010     -0.4   0.2     21          17             1            19
# 5: 2008   0.014     -1.4   1.8    125          67             1           103
# 6: 2009   0.006     -2.0   0.7    115          72             1            90
# 7: 2010   0.010     -0.7   0.2     20          20             1            20
# 8: 2011   0.020     -0.7   0.5     23          19             1            22
# 9: 2012   0.000     -0.5   0.0     24          23             1            18
# 10: 2013   0.041     -0.4   0.9     21          19             1            17
# 11: 2014   0.007     -0.5   0.2     26          21             1            22
# 12: 2015   0.006     -0.3   0.3     47          41             1            37
# 13: 2016  -0.002     -1.4  -0.1     68          50             1            51
# 14: 2017   0.051     -0.6   5.3    104          72             1            84
# 15: 2018   0.043     -0.7   7.7    179         131             1           136
# 16: 2019   0.021     -1.7   4.3    200         131             1           153
# 17: 2020   0.032     -1.7  13.9    431         207             1           318
# 18: 2021   0.035     -1.6  12.1    350         179             1           251
# 19: 2022   0.022     -1.0   2.8    124          71             1           106
prices[
  (close/open)>1.20 & close>7 & (open/lag1close)>1 & 
    volume>100000][order(close*volume,decreasing=T),.SD[1:3],date]%>% 
  with(performance(date,1-lead1close/lead1open,1,symbol))


#At open, sell stocks that climbed yesterday too much
#####


###### volumeshort
# prices[lag1volume/volume_avg>7.5 & lag1_day_delta>.975 & night_delta>1.01  & open>7.5 & 
#          lag1volume%between%c(10000,100000),
#        .(mean(day_delta,na.rm=T),.N), year(date)][order(year)]
#
######

###### volumelong -- incorporated into updownmorn
# prices[lag1volume/volume_avg <.75 & night_delta< .97  & close>5 & 
#          lag1volume%between%c(10000,100000),
#        .(mean(day_delta,na.rm=T),.N), year(date)][order(year)]
# ###### works for ADRCs with less conservative threshold
# prices[lag1volume/volume_avg <1 & night_delta< .96 & 
#          volume_avg*lag1close>100000 & volume_avg*lag1close<1000000 & 
#          volume_avg>50000,
#        .(mean(day_delta,na.rm=T),.N), year(date)][order(year)]
######



sq=function(x)x^2

# Regression strategy
prices[,reg_predict := as.numeric(32)]
prices[,reg_predict := NA]
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
window = 100
prices[,lagging_corr_long:=NULL]
prices[symbol %in% prices[days_around>window, unique(symbol)], 
       lagging_corr_long:=
         runCor( close/open, open/lag1close, window),
       symbol]

min_corr = .45
prices[lead1open/close<.97 & lagging_corr_long< -min_corr & spy_future_night_delta>.99 & 
         volume%between%c(10000,50000) & close>7,
          .(mean(lead1close/lead1open,na.rm=T),.N, length(unique(date))), year(date)][order(year)]
prices[lead1open/close>1.03 & lagging_corr_long< -min_corr & 
         volume%between%c(10000,50000) & close>7,
       .(mean(lead1close/lead1open,na.rm=T),.N, length(unique(date))), year(date)][order(year)]

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



######
rally_avg(prices,100)
# perf drawdown days_traded
# 1: 0.02611111     -2.1         698
# year average drawdown total trades days_traded avg_days_held stocks_traded
# 1: 2005   0.021      0.0   0.0      2           2      9.000000             2
# 2: 2006   0.019     -0.1   0.2     10           9      3.300000             7
# 3: 2007   0.065     -0.1   1.6     25          18      7.080000            19
# 4: 2008   0.039     -0.7   6.9    177          66      6.570621            99
# 5: 2009   0.007     -1.4   2.0    306          78      6.150327           121
# 6: 2010   0.022     -0.1   0.9     44          27      4.386364            32
# 7: 2011   0.030     -0.2   1.3     45          28      5.755556            32
# 8: 2012   0.014     -0.3   0.6     47          26     27.787234            30
# 9: 2013   0.028     -0.2   1.0     36          23      9.750000            16
# 10: 2014   0.043     -0.2   1.3     30          23      9.100000            18
# 11: 2015   0.008     -0.6   0.4     58          36      5.241379            29
# 12: 2016  -0.013     -2.1  -1.2     91          40      5.978022            51
# 13: 2017   0.007     -1.5   0.3     43          33      4.581395            28
# 14: 2018   0.082     -1.2   2.9     36          27      7.666667            25
# 15: 2019   0.009     -1.0   1.1    117          63      6.786325            67
# 16: 2020   0.026     -1.6   7.0    271          83      6.036900           157
# 17: 2021   0.030     -1.3   6.8    226          74      6.243363            99
# 18: 2022   0.033     -1.4   3.7    111          42      7.711712            65

prices[close>7 & volume>5000000 & wday(date) %in% c(6,2) &
         close<lag1high & sell_rally_day>6 & 
         ((sell_rally_avg-avg_delta)/sell_rally_avg) %between% c(.02,.05)][
         ][order(-high/close),head(.SD,5),date] %>%
  rbind(
    prices[close>7 & volume>5000000 & wday(date) %in% c(6,2) &
             close>lag1high & sell_rally_day<2 & 
             ((sell_rally_avg-avg_delta)/sell_rally_avg) < -.03][
             ][order(high/close),head(.SD,5),date]
  )%>%
  with(performance(date,
                   ifelse(close<lag1high,lead1sellrally/lead1open-1,1-lead1sellrally/lead1open),
                   lead1sellrallydate-date,symbol))

##############
# nightbot

prices[close>5 & volume>100000 & lead1open/close>1.15 & spy_future_night_delta<1.005 ]%>%
  with(performance(date,1-lead1close/lead1open,
                   1,symbol))
