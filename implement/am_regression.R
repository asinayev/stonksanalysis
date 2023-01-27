args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)

prices = fread('/tmp/prices.csv')
prices = only_passing(prices, min_volume=75000, min_close=7, last_n = F)

lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5, rolling_features=F)
regression_features(prices)

lm1 = lm(future_day_delta~
           day_delta + night_delta + day_fall + day_rise
         ,prices, weights = (prices$date-min(prices$date))/as.integer(max(prices$date-min(prices$date))),
         subset = date>Sys.Date()-3*365 
)

prices[,reg_predict:=predict(lm1, prices)]
prices[,reg_predict:=ifelse(is.na(reg_predict),1,reg_predict)]

prices[volume>75000 & close>7,
       threshold:=pmin(quantile(reg_predict,.001,type=1),.995), date]

prices[date==max(date, na.rm=T) & 
         reg_predict<threshold,
       .(date, symbol, close, volume)] %>%
  dplyr::mutate( action='SELL', order_type='MKT', time_in_force='OPG') %>%
  write_strat(strat_name='regression')
