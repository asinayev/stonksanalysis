args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)
prices = fread('/tmp/prices.csv', colClasses = c('cik'='character'))

lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)

prices=get_financials(prices)

prices[date==max(date, na.rm=T) & 
         close>7 & avg_volume>250000 & 
         avg_delta>.99 & 
         (mean_eps/close) %between% c(.15,100) & eps_unit=="USD / shares" &
         (((close-low)/avg_range)<.15 )][
           order(close/lag1close,decreasing=F)] %>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='earners')
