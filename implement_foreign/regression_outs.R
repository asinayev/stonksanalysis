require(tidyquant, quietly = T)
require(data.table, quietly = T)

prices = fread('/tmp/prices.csv')

prices = prices[symbol %in% prices[!is.na(close) & !is.na(open),.N,symbol][N>365, symbol]]
setorder(prices, symbol, date)
prices[,c("lag1close", "lag2close"):=shift(close, n = 1:2, type = "lag"),symbol]
prices[,c("lag1open",  "lag2open" ):=shift(open,  n = 1:2, type = "lag"),symbol]
prices[,c("lag1high",  "lag2high" ):=shift(high,  n = 1:2, type = "lag"),symbol]
prices[,c("lag1low",   "lag2low"  ):=shift(low,   n = 1:2, type = "lag"),symbol]
prices[,day_delta:= close/open]
prices[,day_fall:= low/open]
prices[,day_rise:= high/open]
prices[,night_delta:= open/lag1close]
prices[!is.na(volume),volume_avg:= SMA(shift(volume,1,type='lag'), n = 30, ),symbol ]
prices[,c("lag1_day_delta",    "lag2_day_delta" , "future_day_delta"  ):=
         shift(day_delta,    n = c(1,2,-1), type = "lag"),symbol]
prices[,c("lag1_night_delta",  "lag2_night_delta" ):=shift(night_delta,  n = 1:2, type = "lag"),symbol]
prices[,c("lag1_day_fall",    "lag2_day_fall"   ):=shift(day_fall,    n = 1:2, type = "lag"),symbol]
prices[,c("lag1_day_rise",    "lag2_day_rise"   ):=shift(day_rise,    n = 1:2, type = "lag"),symbol]

prices[!is.na(day_delta) & !is.na(lag1_day_rise),
       corr_lag_rise:=
         runCor( day_delta, lag1_day_rise, 7),
       symbol]
prices[!is.na(day_delta) & !is.na(lag1_day_delta),
       corr_lag_delta:=
         runCor( day_delta, lag1_day_delta, 7),
       symbol]
prices[!is.na(day_delta) & !is.na(lag1_day_fall),
       corr_lag_fall:=
         runCor( day_delta, lag1_day_fall, 7),
       symbol]

prices[!is.na(day_delta) & !is.na(lag1_day_rise),
       corr_lag_rise_long:=
         runCor( day_delta, lag1_day_rise, 350),
       symbol]
prices[!is.na(day_delta) & !is.na(lag1_day_delta),
       corr_lag_delta_long:=
         runCor( day_delta, lag1_day_delta, 350),
       symbol]
prices[!is.na(day_delta) & !is.na(lag1_day_fall),
       corr_lag_fall_long:=
         runCor( day_delta, lag1_day_fall, 350),
       symbol]

lm1 = lm(future_day_delta~
           corr_lag_fall*day_fall * log(volume_avg)+
           corr_lag_delta*day_delta * log(volume_avg)+
           corr_lag_rise*day_rise * log(volume_avg)+
           corr_lag_fall_long*day_fall * log(volume_avg)+
           corr_lag_delta_long*day_delta * log(volume_avg)+
           corr_lag_rise_long*day_rise * log(volume_avg),
         prices,
         subset = log(volume_avg*close+1) > 15 &
           date>Sys.Date()-365
)


#print out all of last week's picks
prices[date>Sys.Date()-14 & 
         log(volume_avg*lag1close+1)>15 & 
         (predict(lm1, prices) < .985) ,
       .(date, ticker=symbol, closingprice=close, future_day_delta)][order(date)]

prices[date==max(date, na.rm=T) & 
         log(volume_avg*lag1close+1)>15 & 
         predict(lm1, prices) < .985 ,
       .(date, ticker=symbol, price=close)] %>%
  fwrite('/tmp/predicted_short_stocks.csv')
