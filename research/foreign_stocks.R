require(tidyquant)
require(data.table)
require(rpart)
require(ggplot2)

setwd('~/stonksanalysis')
source("polygon.R", local=T)
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

prices=lapply(Sys.Date()-365*6:1, sampled_data, key=POLYKEY, ticker_type='CS', details=T) %>%   
  rbindlist(fill=T) %>%
  dplyr::rename(symbol=stock, close=AdjClose, date=Date)

setorder(prices, symbol, date)
prices = prices[!is.na(volume) & !is.na(close) & !is.na(open)]
prices[,days_around:=cumsum(!is.na(close)),symbol]

prices[,c("lag1close", "lag2close", "lead1close"):=shift(close, n = c(1:2,-1), type = "lag"),symbol]
prices[,c("lag1open",  "lag2open", "lead1open"):=shift(open,  n = c(1:2,-1), type = "lag"),symbol]
prices[,c("lag1high",  "lag2high", "future_day_high" ):=shift(high,  n = c(1,2,-1), type = "lag"),symbol]
prices[,c("lag1low",   "lag2low"  ):=shift(low,   n = 1:2, type = "lag"),symbol]
prices[,c("lag1volume"  ):=shift(volume,   n = 1, type = "lag"),symbol]
prices[,day_delta:= close/open]
prices[,day_fall:= low/open]
prices[,day_rise:= high/open]
prices[,night_delta:= open/lag1close]
prices[symbol %in% prices[days_around>30, unique(symbol)],
       volume_avg:= SMA(shift(volume,1,type='lag'), n = 30, ),symbol ]
prices[symbol %in% prices[days_around>30, unique(symbol)],
       close_avg:= SMA(shift(close,1,type='lag'), n = 30, ),symbol ]
prices[,low_running:= frollapply(close, min, n = 50 ),symbol ]
prices[,c("lag1_day_delta",    "lag2_day_delta" , "future_day_delta"  ):=
         shift(day_delta,    n = c(1,2,-1), type = "lag"),symbol]
prices[,c("lag1_night_delta",  "lag2_night_delta" , "future_night_delta" ):=
         shift(night_delta,  n = c(1,2,-1), type = "lag"),symbol]
prices[,c("lag1_day_fall",    "lag2_day_fall"   ):=shift(day_fall,    n = 1:2, type = "lag"),symbol]
prices[,c("lag1_day_rise",    "lag2_day_rise", "future_day_rise"   ):=shift(day_rise,    n = c(1:2,-1), type = "lag"),symbol]
prices[,future_day_delta_ltd:=ifelse(future_day_rise>1.2, 1.2, future_day_delta )]


#####bandlong
# prices[!is.na(lag1close),
#        c('lower','avg','upper','pctB'):= data.frame(BBands(lag1close, n = 30, EMA, sd=2.5)),
#        symbol ]

prices[(close<low*1.01 | close==low_running) & day_delta<.85 & close>5
       & volume_avg*lag1close>100000  & volume_avg*lag1close<10000000, 
        .(mean(lead1open/close, na.rm=T), median(lead1open/close,na.rm=T),.N),
       year(date)][order(year)]

#At close, buy stocks that fell 15% and closed at the day's low or 50D low of close prices
#####

#####updown
prices[
  open/lag1close> 1.05 & close/open<.9  & close>5 & 
    volume*lag1close>100000  & volume*lag1close<1000000
  ,.(mean(lead1open/close, na.rm=T), .N), 
  .(year(date))][order(year)] 

#At close, buy stocks that climbed last night and fell today, setting limit to 95% of the open
#####

#####updownmorn
prices[
  night_delta< .975 & lag1_day_delta>1.025  & open>5 & 
    lag1volume*lag1close>75000 & lag1volume*lag1close<500000
  ,.(mean(day_delta, na.rm=T), .N), 
  .(year(date))][order(year)] 

prices[
  night_delta< .975 & lag1_day_delta>1.05 & 
    volume_avg*lag1close>100000  & lag1volume*lag1close<500000 & 
    date>'2022-01-01',
                           .(date,ticker=symbol,open,delta=day_delta)] %>%
  wins_by_hour

#At open, buy stocks that climbed yesterday but fell overnight today
#####



###### volumeshort
prices[lag1volume/volume_avg>7.5 & lag1_day_delta>.975 & night_delta>1.01  & open>7.5 & 
         volume_avg*lag1close>75000  & volume_avg>50000,
       .(mean(day_delta,na.rm=T),.N), year(date)][order(year)]
#
######

###### volumelong 
prices[lag1volume/volume_avg <.75 & night_delta< .97  & close>5 & 
         volume_avg*lag1close>100000 & volume_avg*lag1close<1000000 & 
         volume_avg>50000,
       .(mean(day_delta,na.rm=T),.N), year(date)][order(year)]
###### works for ADRCs with less conservative threshold
prices[lag1volume/volume_avg <1 & night_delta< .96 & 
         volume_avg*lag1close>100000 & volume_avg*lag1close<1000000 & 
         volume_avg>50000,
       .(mean(day_delta,na.rm=T),.N), year(date)][order(year)]
######

prices[symbol %in% prices[days_around>7, unique(symbol)], 
       lagging_corr:=
         runCor( day_delta, night_delta, 7),
       symbol]

prices[symbol %in% prices[days_around>7, unique(symbol)],
       corr_lag_rise:=
         runCor( day_delta, lag1_day_rise, 7),
       symbol]
prices[symbol %in% prices[days_around>7, unique(symbol)],
       corr_lag_delta:=
         runCor( day_delta, lag1_day_delta, 7),
       symbol]
prices[symbol %in% prices[days_around>7, unique(symbol)],
       corr_lag_fall:=
         runCor( day_delta, lag1_day_fall, 7),
       symbol]

prices[symbol %in% prices[days_around>350, unique(symbol)],
       corr_lag_rise_long:=
         runCor( day_delta, lag1_day_rise, 350),
       symbol]
prices[symbol %in% prices[days_around>350, unique(symbol)],
       corr_lag_delta_long:=
         runCor( day_delta, lag1_day_delta, 350),
       symbol]
prices[symbol %in% prices[days_around>350, unique(symbol)],
       corr_lag_fall_long:=
         runCor( day_delta, lag1_day_fall, 350),
       symbol]

sq=function(x)x^2

# Regression strategy
prices[,reg_predict := as.numeric(32)]
prices[,reg_predict := NA]
for (yr in 2019:2021 ){
  IS = prices[year(date) %between% c(yr-3, yr-1) ]
  lm1 = lm(future_day_delta_ltd ~
             corr_lag_fall_long*day_fall +
             corr_lag_delta_long*day_delta +
             corr_lag_rise_long*day_rise
           ,IS, weights = (IS$date-min(IS$date))/as.integer(max(IS$date-min(IS$date)))
  )
  prices[year(date)==yr, reg_predict:=predict(lm1,data.frame(.SD))  ]
  gc()
}


IS = prices[date>Sys.Date()-3*365 & date<Sys.Date()-30]
lm1 = lm(future_day_delta_ltd ~
           corr_lag_fall_long*day_fall +
           corr_lag_delta_long*day_delta +
           corr_lag_rise_long*day_rise
         ,IS, weights = (IS$date-min(IS$date))/as.integer(max(IS$date-min(IS$date)))
)
prices[year(date)==2022,
       reg_predict:=predict(lm1,data.frame(.SD))  ]

prices[reg_predict<.99  & 
         volume_avg*close>100000,
       .(mean(future_day_delta,na.rm=T),.N), 
       year(date)][order(year)]

prices[!is.na(future_day_delta) & reg_predict<.99  & 
         log(volume_avg*close+1) %between% c(15,25)][order(date, symbol)][ 
           ,.(date, MA = EMA(future_day_delta,na.rm=T,50))] %>% with(plot(date, MA, type='l', ylim=c(.8,1.2)))
x_ <- c(1, .99, 1.01,.95,1.05) %>% lapply( function(x)abline(h=x))


# Overnight strategy makes money over 50 trades (either long or short) with few exceptions
window = 100
prices[,lagging_corr_long:=NULL]
prices[symbol %in% prices[days_around>window, unique(symbol)], 
       lagging_corr_long:=
         runCor( day_delta, night_delta, window),
       symbol]

min_corr = .5
prices[future_night_delta<.97 & lagging_corr_long< -min_corr & 
         volume*close>100000 & close>5,
          .(mean(future_day_delta,na.rm=T),.N), year(date)][order(year)]
prices[future_night_delta>1.03 & lagging_corr_long< -min_corr & 
         volume*close>100000 & close>5,
       .(mean(future_day_delta,na.rm=T),.N), year(date)][order(year)]

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
