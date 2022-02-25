require(tidyquant, quietly = T)
require(data.table, quietly = T)

prices = fread('/tmp/prices.csv')

prices = prices[symbol %in% prices[!is.na(close) & !is.na(open),.N,symbol][N>365, symbol]]
setorder(prices, symbol, date)
prices[,c("lag1close", "lag2close"):=shift(close, n = 1:2, type = "lag"),symbol]
prices[,day_delta:= close/open]
prices[,night_delta:= open/lag1close]
prices[!is.na(volume),volume_avg:= SMA(volume, n = 30), symbol ]
prices[,lag1volume:=shift(close, n = 1, type = "lag"),symbol]
prices[,c("lag1_day_delta",    "lag2_day_delta" , "future_day_delta"  ):=
         shift(day_delta,    n = c(1,2,-1), type = "lag"),symbol]
prices[,c("lag1_night_delta",  "lag2_night_delta" , "future_night_delta" ):=
         shift(night_delta,  n = c(1,2,-1), type = "lag"),symbol]


prices[!is.na(day_delta) & !is.na(night_delta),
       lagging_corr:=
         runCor( day_delta, night_delta, 364),
       symbol]

prices[date==max(date, na.rm=T) & 
         volume*close>75000 & 
         lagging_corr< -.4 ,
       .(date, symbol, close,
         buy = trunc(close*96,3)/100 , sell = (trunc(close*104,3)+1)/100)] %>%
  fwrite('/tmp/correlated_stocks.csv')


prices[date==max(date, na.rm=T) & close/open>1.05 &
         volume*close>75000 & volume*close<500000,
       .(date, symbol, close)] %>%
  dplyr::mutate( stock=symbol, action='BUY', 
                 strike_price=trunc(close*975,3)/1000, 
                 order_type='LMT', time_in_force='OPG') %>%
  fwrite('/tmp/updownmorn.csv')

prices[date==max(date, na.rm=T) & 
         volume/volume_avg>10 & day_delta>.975 & 
         volume_avg*close>75000  & volume_avg>50000,
       .(date, symbol, close)] %>%
  dplyr::mutate( stock=symbol, action='SELL', 
                 strike_price=trunc(close*1010,3)/1000, 
                 order_type='LMT', time_in_force='OPG') %>%
  fwrite('/tmp/volumeshort.csv')

prices[date==max(date, na.rm=T) & 
         volume/volume_avg <.5 & 
         volume_avg*close>100000 & volume_avg*close<1000000 & 
         volume_avg>50000,
       .(date, symbol, close)] %>%
  dplyr::mutate( stock=symbol, action='BUY', 
                 strike_price=trunc(close*96,3)/1000, 
                 order_type='LMT', time_in_force='OPG') %>%
  fwrite('/tmp/volumelong.csv')

