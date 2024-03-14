args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)
prices = fread('/tmp/prices.csv', colClasses = c('cik'='character'))

prices = only_passing(prices, min_volume=0, min_close=0, last_n = 150)

lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)

prices=get_financials(prices)

prices[date==max(date, na.rm=T) & 
         close>7 & avg_volume>200000 & 
         mean_eps/close >.25 & eps_unit=="USD / shares" &
         (((close-low)/avg_range)<.15 )][
           order(avg_volume,decreasing=T)] %>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='earners')
