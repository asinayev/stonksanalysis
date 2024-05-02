args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)
prices = fread('/tmp/prices.csv')

prices = only_passing(prices, min_volume=0, min_close=0, last_n = 150)

prices[symbol %in% prices[,.N,symbol][N>100,symbol]
       ,max_volume:= zoo::rollapply(volume,max,width=100, align='right',fill=NA),symbol ]
prices[symbol %in% prices[,.N,symbol][N>25,symbol]
       ,avg_vp:= frollmean(close*volume ,n = 25, align='right',fill=NA),symbol ]
prices[order(avg_vp,    decreasing=T),vp_order :=seq_len(.N),date]
prices[order(market_cap,decreasing=T),cap_order:=seq_len(.N),date]

lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)
# rally_avg(prices,100)

bigcaps = prices[volume>500000 & close>7 & cap_order<200 & vp_order>50]
bigcaps[,bigcap_avg_delta:=mean(avg_delta),date]
bigcaps[,bigcap_avg_delta_short:=mean(avg_delta_short),date]

prices[date==max(date, na.rm=T) & 
         close/open>1.2 & open/lag1close>1 &
         vp_order<3000 & close>7][
  order(day_rise_norm,decreasing=T)] %>%
  head(1) %>%
  dplyr::mutate( action='SELL', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='overbought')

bigcaps[ date==max(date, na.rm=T) & 
           (avg_delta>1.0075 | avg_delta>bigcap_avg_delta*1.0075) & 
           (avg_delta_short>1.015 | avg_delta_short>bigcap_avg_delta_short*1.015)][
            order(day_rise_norm,decreasing=T)]%>%
  head(1) %>%
  dplyr::mutate( action='SELL', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='bigcap_short')

bigcaps[ date==max(date, na.rm=T) & 
           (avg_delta>bigcap_avg_delta*.995 | avg_delta>.995) & 
           (avg_delta_short<bigcap_avg_delta_short*.98 | ((MACD_slow - MACD) > .05))][
           order(day_drop_norm, decreasing=F)]%>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='bigcap_long')

prices[date==max(date, na.rm=T) & 
         close>7 & avg_volume>1000000 & 
         close<lag1high & sell_rally_day>4 & 
         avg_delta<.975][
  order(day_drop_norm, decreasing=F)] %>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='correlated_long')


prices[date==max(date, na.rm=T) & 
         close>7 & avg_volume>500000 & 
             close>lag1high & sell_rally_day<2 & 
             avg_delta_short>1.1][
  order(day_rise_norm,decreasing=T)] %>%
  head(1) %>%
  dplyr::mutate( action='SELL', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='correlated_short')


prices[((low<running_low*1.001)|(avg_delta_short<avg_delta*.98)) &  
         cap_order<50 &
         (((close-low)/avg_range)<.15 ) & 
         date==max(date, na.rm=T)][
         order(day_drop_norm, decreasing=F)]  %>%
 head(1) %>%
 dplyr::mutate( action='BUY',
                order_type='MKT',
                time_in_force='OPG') %>%
 write_strat(strat_name='megacap')


prices[close>5 & volume>100000 & 
         (volume>=max_volume & avg_delta_short<.99) & 
         (log(vp_order)-log(cap_order))>.4 &
         (((close-low)/avg_range)<.2 ) &
         date==max(date, na.rm=T)][
           order(day_drop_norm, decreasing=F)]%>%
  head(1)%>%
  dplyr::mutate( action='BUY',
                 order_type=ifelse(volume>250000,'MKT','Adaptive'),
                 time_in_force=ifelse(volume>250000,'OPG','DAY')) %>%
  write_strat(strat_name='volumelong')



prices[date==max(date, na.rm=T) ] %>%
  fwrite('/tmp/stonksanalysis/all_stocks.csv')
