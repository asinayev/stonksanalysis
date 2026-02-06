args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)
prices = fread('/tmp/prices.csv')

prices = add_short_interest(prices,POLYKEY)

prices = only_passing(prices, min_volume=0, min_close=0, last_n = 150)
lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)

prices[ date==max(date, na.rm=T) & 
          vp_order<500 & close>7 & 
          avg_delta>1.01 & avg_delta_short>1.03 &
          short_interest>100000 & days_to_cover==1][
            order(date,day_drop_norm/sd_from0, decreasing=F)][
             order(volume, decreasing=F)]%>%
  head(1) %>%
  dplyr::mutate( action='SELL', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='short_no_interest')
