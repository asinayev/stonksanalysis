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

lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)
prices[,lead1sell_rally:= shift(sell_rally,1,type='lead'),symbol]
prices[,lead1sell_rallydate:= shift(sell_rally_date,1,type='lead'),symbol]

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
  (close/open)>1.20 & close>7 & (open/lag1close)>1 & spy_future_night_delta<1.005 &
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

prices[symbol %in% prices[,.N,symbol][N>100,symbol]
          ,max_volume:= zoo::rollapply(volume,max,width=100, align='right',fill=NA),symbol ]
# did not fall too far?
prices[close>6 & avg_volume>1000000 & volume==max_volume & lag1volume<avg_volume*2 & 
         close/lag1close>.99 & avg_delta_short<.9925][
  order(avg_delta),head(.SD,5),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,1,symbol))

prices[close>6 & avg_volume>1000000 & volume>=pmin(max_volume,avg_volume*3) & 
         close/lag1close>pmin(avg_delta_short^.1,.995) & avg_delta_short<.99][
  order(avg_delta),head(.SD,5),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,1,symbol))

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
# perf drawdown drawdown_days days_traded
# 1: 0.032     -8.5          1010        1583
# year average drawdown total trades days_traded avg_days_held stocks_traded
# 1: 2005   0.018     -0.4   0.7     37          33      3.837838            13
# 2: 2006   0.059     -0.2   1.8     31          20      3.322581            14
# 3: 2007   0.011     -5.9   1.3    114          61      5.736842            39
# 4: 2008   0.018     -6.4   7.9    430         160      6.725581           171
# 5: 2009   0.014     -6.0   2.1    149          76      6.255034            70
# 6: 2010   0.041     -2.9   1.4     34          34      5.000000            19
# 7: 2011   0.033     -1.8   1.9     59          45      4.135593            34
# 8: 2012   0.086     -0.1   2.8     33          30      6.818182            14
# 9: 2013   0.052     -0.7   1.3     25          24      5.560000            16
# 10: 2014   0.035     -1.6   3.6    104          70      6.442308            34
# 11: 2015   0.035     -2.1   4.6    130          85      6.753846            43
# 12: 2016   0.006     -4.2   1.0    154          91      8.129870            50
# 13: 2017   0.062     -1.9   5.3     86          63      7.779070            55
# 14: 2018   0.025     -2.4   4.4    181         118      7.381215            97
# 15: 2019   0.010     -1.9   2.0    195         131      8.117949           105
# 16: 2020   0.046     -2.0  29.2    635         203      6.729134           330
# 17: 2021   0.011     -8.5   6.5    569         222      6.861160           273
# 18: 2022   0.014     -3.4   4.6    322         117      7.416149           125

prices[close>7 & volume>500000 & 
         close<lag1high & sell_rally_day>6 & 
         avg_delta<.975][
         ][order(high/close, decreasing=T),head(.SD,5),date] %>%
  rbind(
    prices[close>7 & volume>500000 & 
             close>lag1high & sell_rally_day<2 & 
             avg_delta_short>1.1][
             ][order(days_around, decreasing=T),head(.SD,5),date]
  )%>%
  with(performance(date,
                   ifelse(close<lag1high,lead1sell_rally/lead1open-1,1-lead1sell_rally/lead1open),
                   lead1sell_rallydate-date,symbol))


#############
# nightbot

prices[close>5 & volume>100000 & lead1open/close>1.15 & spy_future_night_delta<1.005 ]%>%
  with(performance(date,1-lead1close/lead1open,
                   1,symbol))

##############
# megacap
prices[order(market_cap,decreasing=T),cap_order:=seq_len(.N),date]
prices[low<running_low*1.001 & cap_order<10 & 
         lead1sell_rally/lead1open<1.5][
           order(avg_delta_short,decreasing = F),head(.SD,5),date] %>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol))



##############
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
