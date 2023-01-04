args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)
prices = fread('/tmp/prices.csv')

lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)
rally_avg(prices,100)

prices[date==max(date, na.rm=T) & 
         (volume/avg_volume <.75 | close/open>1.025) & 
         volume%between%c(10000,20000) & close>5 ,
       .(date, symbol, close)] %>%
  dplyr::mutate( stock=symbol, action='BUY', 
                 strike_price=trunc(close*975,3)/1000, 
                 order_type='LMT', time_in_force='OPG') %>%
  write_strat(strat_name='updownmorn')

prices[date==max(date, na.rm=T) & 
         close/open>1.2 & open/lag1close>1 &
         volume>100000 & close>7 ,
       .(date, symbol, close, volume)][order(close*volume,decreasing=T)] %>%
  head(3) %>%
  dplyr::mutate( action='SELL', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='overbought')

prices[date==max(date, na.rm=T) & 
         close>7 & volume>5000000 & 
         close<lag1high & sell_rally_day>6 & 
         ((sell_rally_avg-avg_delta)/sell_rally_avg) %between% c(.02,.05) ,
       .(date, symbol, close, volume, high, days_around)][order(days_around,decreasing=T)] %>%
  head(5) %>%
  dplyr::mutate( action='BUY', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='correlated_long')


prices[date==max(date, na.rm=T) & 
         close>7 & volume>5000000 & 
         close>lag1high & sell_rally_day<2 & 
         ((sell_rally_avg-avg_delta)/sell_rally_avg) < -.03 ,
       .(date, symbol, close, volume, high, days_around)][order(days_around,decreasing=T)] %>%
  head(5) %>%
  dplyr::mutate( action='SELL', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='correlated_short')