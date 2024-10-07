args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)
prices = fread('/tmp/prices.csv', colClasses = c('cik'='character'))

prices=get_financials(prices)
setorder(prices, symbol, date)

lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)

prices[date==max(date, na.rm=T) & 
         close>7 & avg_volume>250000 & 
         ( ((MACD_slow - MACD) > .1) | (low<running_low*1.001) | 
             (avg_delta_short<avg_delta*.98) | (sell_rally_day>10)) & 
         (mid_eps/close) >.2 &  eps_unit=="USD / shares"   ][
           order(mid_eps/close, decreasing=T)] %>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='earners')

prices[date==max(date, na.rm=T) & 
         close>7 & avg_volume>250000 & 
         avg_delta<1 & avg_delta_short<.995 &
         (mid_eps/close) >.05 &  eps_unit=="USD / shares"   ][
           order(mid_eps/close, decreasing=T)] %>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='earners_long')