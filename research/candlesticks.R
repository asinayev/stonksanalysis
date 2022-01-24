require(tidyquant)
require(data.table)
require(rpart)
require(ggplot2)

prices=lapply(Sys.Date()-365*8:9, sampled_data, key=POLYKEY, nsample=100, exchange = c('XNAS')) %>%
  rbindlist
  
setorder(prices, symbol, date)

prices[,c("lag1close", "lag2close", "lead1close"):=shift(close, n = c(1:2,-1), type = "lag"),symbol]
prices[,c("lag1open", "lag2open", "lead1open"):=shift(open, n = c(1:2,-1), type = "lag"),symbol]
prices[,delta:= close/lag1close]
prices[,day_delta:= close/open]
prices[,night_delta:= open/lag1close]
prices[,lead1delta:=shift(delta, n = 1, type = "lead"),symbol]
prices[,lead1daydelta:=shift(day_delta, n = 1, type = "lead"),symbol]
prices[,lead1nightdelta:=shift(night_delta, n = 1, type = "lead"),symbol]

prices[,switcher:=date>date[.N-5] | date<date[3],
       .(symbol,month(date),year(date))]
prices[,lag_switcher:=shift(switcher,   n = 1, type = "lag")]
prices[!is.na(switcher) & !is.na(lag_switcher),
       switcher2:=cumsum(switcher!=lag_switcher)]
prices[, 
       .(sum(delta-1, na.rm=T), 
         sum(ifelse(switcher, delta-1, 0), na.rm=T), 
         sum(ifelse(switcher, day_delta-1, 0), na.rm=T), 
         sum(ifelse(switcher, night_delta-1, 0), na.rm=T)),
       .(symbol,year(date))][order(symbol,year)]

prices[lag1close/lag1open>1.01 & open/lag1close<.99  & 
          volume*close>100000
       ,
       .(mean(lead1delta, na.rm=T), 
         mean(lead1daydelta, na.rm=T), 
         mean(lead1nightdelta, na.rm=T), .N), 
       .(year(date), 
         overall_rise = round(lag1open/close,2),
         delta = round(close/open,2))][N>100][order(year, pmax(-V1,-V2,-V3))] %>%
  dplyr::mutate(V1=ifelse(V3>1.03, 1.03, V3) ) %>%
  ggplot(aes(x=delta, y=overall_rise, color=V1, size=log(N)))+
  scale_color_gradient2(
    low = "blue",
    mid = "white",
    high = "yellow",
    midpoint = 1
  )+
  geom_point() + facet_wrap(~year)



prices[lag1close/lag1open>1.02 & open/close<.98 & 
         open/lag1close> .98 & lag1open/close>.95 & 
         volume*close>100000 & volume*close<1000000,
       .(mean(lead1delta, na.rm=T), 
         mean(lead1daydelta, na.rm=T), 
         mean(lead1nightdelta, na.rm=T), .N), 
       .(year(date))][order(year)] 