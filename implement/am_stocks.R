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
rally(prices)
# rally_avg(prices,100)

bigcaps = prices[volume>500000 & close>7 & cap_order<250]

bigcaps[ date==max(date, na.rm=T) & 
           ((avg_delta>.995 & avg_delta_short<.975) | (close>open*1.05 & avg_delta_short<1))][
           order(volume, decreasing=F)]%>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='bigcap_long')

prices[date==max(date, na.rm=T) & 
         close>7 & avg_volume>1000000 & 
         close<lag1high & sell_rally_day>4 & 
         avg_delta<.975][
  order(day_drop_norm/sd_from0, decreasing=F)] %>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='correlated_long')


prices[avg_delta_short<avg_delta*.985 &  
         cap_order<50 & lagging_corr_long>.7 & 
         date==max(date, na.rm=T)][
         order(day_drop_norm/sd_from0, decreasing=F)]  %>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
 write_strat(strat_name='megacap')


prices[close>7 & volume>100000 & 
         volume>=max_volume & 
         avg_delta_short<.99 & 
         vp_order>cap_order &
         (close-low)/avg_range<.1  &
         date==max(date, na.rm=T)][
           order(day_drop_norm/sd_from0, decreasing=F)]%>%
  head(1)%>%
  dplyr::mutate( action='BUY', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='volumelong')


prices[date==max(date, na.rm=T) ] %>%
  fwrite('/tmp/stonksanalysis/all_stocks.csv')
