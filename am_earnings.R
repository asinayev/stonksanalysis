args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)
prices = fread('/tmp/prices.csv')

prices = only_passing(prices, min_volume=0, min_close=0, last_n = 150)

lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)

prices=get_financials(prices)

prices[date==max(date, na.rm=T) & 
         close>7 & avg_volume>200000 & 
         ((low<running_low*1.015)|(avg_delta_short<avg_delta*.995)) &  
         mean_eps/close >.2 & eps_unit=="USD / shares" &
         (((close-low)/avg_range)<.2 )][
           order(market_cap,decreasing=F)] %>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='MIDPRICE',
                 time_in_force='DAY') %>%
  write_strat(strat_name='earners')
