require(tidyquant)
require(data.table)
require(rpart)
require(ggplot2)

fundamentals = fread("stonksanalysis/other_datasources/nasdaq_screener_1638810601422.csv") # from https://www.nasdaq.com/market-activity/stocks/screener
fundamentals = fundamentals[sample(nrow(fundamentals))]

splits = 16
system.time(
  prices <- fundamentals$Symbol %>%
    unique %>% split(1:splits) %>%
    parallel::mclapply(tq_get,
                       from=Sys.Date()-6*365,
                       to=Sys.Date()+2,
                       mc.cores = splits
    ) %>%
    rbindlist(fill=T)
)

prices = prices[symbol %in% prices[!is.na(close) & !is.na(open),.N,symbol][N>365, symbol]]
setorder(prices, symbol, date)
prices[,c("lag1close", "lag2close"):=shift(close, n = 1:2, type = "lag"),symbol]
prices[,c("lag1open",  "lag2open" ):=shift(open,  n = 1:2, type = "lag"),symbol]
prices[,c("lag1high",  "lag2high", "future_day_high" ):=shift(high,  n = c(1,2,-1), type = "lag"),symbol]
prices[,c("lag1low",   "lag2low"  ):=shift(low,   n = 1:2, type = "lag"),symbol]
prices[,day_delta:= close/open]
prices[,day_fall:= low/open]
prices[,day_rise:= high/open]
prices[,night_delta:= open/lag1close]
prices[!is.na(volume),volume_avg:= SMA(shift(volume,1,type='lag'), n = 30, ),symbol ]
prices[,c("lag1_day_delta",    "lag2_day_delta" , "future_day_delta"  ):=
         shift(day_delta,    n = c(1,2,-1), type = "lag"),symbol]
prices[,c("lag1_night_delta",  "lag2_night_delta" , "future_night_delta" ):=
         shift(night_delta,  n = c(1,2,-1), type = "lag"),symbol]
prices[,c("lag1_day_fall",    "lag2_day_fall"   ):=shift(day_fall,    n = 1:2, type = "lag"),symbol]
prices[,c("lag1_day_rise",    "lag2_day_rise", "future_day_rise"   ):=shift(day_rise,    n = c(1:2,-1), type = "lag"),symbol]
prices[,future_day_delta_ltd:=ifelse(future_day_rise>1.2, 1.2, future_day_delta )]

prices[!is.na(day_delta) & !is.na(night_delta),
       lagging_corr:=
         runCor( day_delta, night_delta, 364),
       symbol]

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

sq=function(x)x^2

# Regression strategy

prices[,reg_predict := NA]
for (pricedate in sort(unique(prices$date))[-1:-502] ){
  print(as.Date(pricedate))
  IS = prices[date %between% c(pricedate-600, pricedate-1) & 
                log(volume_avg*close+1) %between% c(15,25)]
  lm1 = lm(future_day_delta_ltd ~
             corr_lag_fall*day_fall * log(volume_avg)+ corr_lag_fall * sq(day_fall) +
             corr_lag_delta*day_delta * log(volume_avg)+ corr_lag_delta * sq(day_delta) +
             corr_lag_rise*day_rise * log(volume_avg)+ corr_lag_rise * sq(day_rise) +
             corr_lag_fall_long*day_fall * log(volume_avg)+ corr_lag_rise_long * sq(day_rise) +
             corr_lag_delta_long*day_delta * log(volume_avg)+corr_lag_delta_long * sq(day_delta) +
             corr_lag_rise_long*day_rise * log(volume_avg) + corr_lag_rise_long * sq(day_rise)
           ,IS
  )
  prices[date==pricedate,
         reg_predict:=predict(lm1,data.frame(.SD))]
  gc()
}
# 2018   .983   208
# 2019   .995    86
# 2020  1.010  2486
# 2021   .974   491

prices[reg_predict<.975  & 
         log(volume_avg*close+1) %between% c(15,25),
       .(mean(future_day_delta_ltd,na.rm=T),.N), 
       year(date)][order(year)]

prices[!is.na(future_day_delta_ltd) & reg_predict<.985  & 
         log(volume_avg*close+1) %between% c(15,25)][order(date, symbol)][ 
           ,.(date, MA = EMA(future_day_delta_ltd,na.rm=T,50))] %>% with(plot(date, MA, type='l', ylim=c(.8,1.2)))
x_ <- c(1, .99, 1.01,.95,1.05) %>% lapply( function(x)abline(h=x))


# Overnight strategy makes money over 50 trades (either long or short) with few exceptions
prices[future_night_delta<.96 & lagging_corr< -.4 & log(volume_avg+1) %between% c(10,25),
          .(mean(future_day_delta_ltd,na.rm=T),.N), year(date)][order(year)]
prices[future_night_delta>1.04 & lagging_corr< -.4 & log(volume_avg+1) %between% c(10,25),
       .(mean(future_day_delta_ltd,na.rm=T),.N), year(date)][order(year)]

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
