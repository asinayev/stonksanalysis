require(tidyquant)
require(data.table)

prices = fread('/tmp/prices.csv')

prices = prices[symbol %in% prices[!is.na(close) & !is.na(open),.N,symbol][N>365, symbol]]
setorder(prices, symbol, date)
prices[,c("lag1close", "lag2close"):=shift(close, n = 1:2, type = "lag"),symbol]
prices[,day_delta:= close/open]
prices[,night_delta:= open/lag1close]
prices[!is.na(volume),volume_avg:= SMA(shift(volume,1,type='lag'), n = 30, ),symbol ]
prices[,c("lag1_day_delta",    "lag2_day_delta" , "future_day_delta"  ):=
         shift(day_delta,    n = c(1,2,-1), type = "lag"),symbol]
prices[,c("lag1_night_delta",  "lag2_night_delta" , "future_night_delta" ):=
         shift(night_delta,  n = c(1,2,-1), type = "lag"),symbol]


prices[!is.na(day_delta) & !is.na(night_delta),
       lagging_corr:=
         runCor( day_delta, night_delta, 364),
       symbol]

prices[future_night_delta<.97 & lagging_corr< -.4 & log(volume_avg+1) %between% c(10,13),
          .(mean(future_day_delta,na.rm=T),.N), year(date)]

prices[future_night_delta>1.03 & lagging_corr< -.4 & log(volume_avg+1) %between% c(10,13),
       .(mean(future_day_delta,na.rm=T),.N), year(date)]

prices[date==max(date, na.rm=T) & 
         lagging_corr< -.4 & 
         log(volume_avg+1) %between% c(10,13),
       .(date, symbol, close,
         buy = trunc(close*97,3)/100 , sell = (trunc(close*103,3)+1)/100)] %>%
  fwrite('/tmp/correlated_stocks.csv')
