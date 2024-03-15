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
         ( ((MACD_slow - MACD) > .03) | (low<running_low*1.005) | 
             (avg_delta_short<avg_delta*.985) | (sell_rally_day>6)) & 
         (mean_eps/close) %between% c(.2, 100) &  eps_unit=="USD / shares"  ][
           order(avg_delta_short,decreasing=F)] %>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='earners')
