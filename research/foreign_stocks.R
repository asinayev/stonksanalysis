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
prices[,c("lag1high",  "lag2high" ):=shift(high,  n = 1:2, type = "lag"),symbol]
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
prices[,c("lag1_day_rise",    "lag2_day_rise"   ):=shift(day_rise,    n = 1:2, type = "lag"),symbol]


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

sq=function(x)x^2

prices[,reg_predict := NA]
for (yr in 2018:2021){
  IS = prices[year(date)==(yr-1) & 
                log(volume_avg*close+1) %between% c(15,25)]
  lm1 = lm(future_day_delta~
             corr_lag_fall*day_fall +
             corr_lag_delta*day_delta +
             corr_lag_rise*day_rise,
           IS
  )
  prices[,reg_predict:=ifelse(year(date)==yr, predict(lm1,prices), reg_predict)]
  gc()
}


# Overnight strategy makes money over 50 trades (either long or short) with few exceptions
prices[reg_predict<.985,
       .(mean(future_day_delta,na.rm=T),.N), year(date)][order(year)]

prices[night_delta<.97 & lagging_corr< -.4][order(date, symbol)][ ,
                                                                  .(date, MA = SMA(day_delta,na.rm=T,50))] %>% with(plot(date, MA, type='l', ylim=c(.5,1.5)))
abline(h=1)
abline(h=0.9)

prices[night_delta>1.03 & lagging_corr< -.4][order(date, symbol)][ 
  ,.(date, MA = SMA(day_delta,na.rm=T,50))] %>% with(plot(date, MA, type='l', ylim=c(.5,1.5)))
abline(h=1)
abline(h=1.1)

# Overnight strategy makes money over 50 trades (either long or short) with few exceptions
prices[future_night_delta<.96 & lagging_corr< -.4 & log(volume_avg+1) %between% c(10,25),
          .(mean(future_day_delta,na.rm=T),.N), year(date)][order(year)]
prices[future_night_delta>1.04 & lagging_corr< -.4 & log(volume_avg+1) %between% c(10,25),
       .(mean(future_day_delta,na.rm=T),.N), year(date)][order(year)]

prices[future_night_delta<.96 & lagging_corr< -.4 & !is.na(future_day_delta) & log(volume_avg+1) %between% c(10,25)][order(date, symbol)][ ,
       .(date, MA = SMA(future_day_delta,na.rm=T,50))] %>% with(plot(date, MA, type='l', ylim=c(.8,1.2)))
abline(h=1)
abline(h=0.99)
abline(h=1.01)

prices[future_night_delta>1.04 & lagging_corr< -.4 & !is.na(future_day_delta) & log(volume_avg+1) %between% c(10,25)][order(date, symbol)][ 
  ,.(date, MA = SMA(future_day_delta,na.rm=T,50))] %>% with(plot(date, MA, type='l', ylim=c(.8,1.2)))
abline(h=1)
abline(h=1.01)
abline(h=0.99)
