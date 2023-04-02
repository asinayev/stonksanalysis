args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)
prices = fread('/tmp/prices.csv')

prices = only_passing(prices, min_volume=10000, min_close=5, last_n = 150)

prices[symbol %in% prices[,.N,symbol][N>100,symbol]
       ,max_volume:= zoo::rollapply(volume,max,width=100, align='right',fill=NA),symbol ]
prices[symbol %in% prices[,.N,symbol][N>25,symbol]
       ,avg_vp:= frollmean(close*volume ,n = 25, align='right',fill=NA),symbol ]
prices[order(avg_vp,    decreasing=T),vp_order :=seq_len(.N),date]
prices[order(market_cap,decreasing=T),cap_order:=seq_len(.N),date]

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
         vp_order<3000 & close>7 ,
       .(date, symbol, close, volume)][order(close*volume,decreasing=T)] %>%
  head(3) %>%
  dplyr::mutate( action='SELL', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='overbought')

prices[ date==max(date, na.rm=T) & 
          volume>500000 & close>7 & 
          avg_delta>1.0075 & avg_delta_short>1.015 &
          cap_order<200 & vp_order>50,
        .(date, symbol, close, volume)][
            order(avg_delta_short,decreasing = T)]%>%
  head(5) %>%
  dplyr::mutate( action='SELL', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='bigcap_short')


prices[date==max(date, na.rm=T) & 
         close>7 & volume>500000 & 
         close<lag1high & sell_rally_day>6 & 
         avg_delta<.975 ,
       .(date, symbol, close, volume, high)][order(high/close,decreasing=T)] %>%
  head(5) %>%
  dplyr::mutate( action='BUY', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='correlated_long')


prices[date==max(date, na.rm=T) & 
         close>7 & volume>500000 & 
             close>lag1high & sell_rally_day<2 & 
             avg_delta_short>1.1 ,
       .(date, symbol, close, volume, high, days_around)][order(days_around,decreasing=T)] %>%
  head(5) %>%
  dplyr::mutate( action='SELL', 
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='correlated_short')


prices[((low<running_low*1.001)|(avg_delta_short<avg_delta*.98)|((MACD_slow - MACD) > .015)) &  
                cap_order<10 &
        date==max(date, na.rm=T)][
          order(avg_delta_short,decreasing = F)]  %>%
 head(3) %>%
 dplyr::mutate( action='BUY',
                order_type='MKT',
                time_in_force='OPG') %>%
 write_strat(strat_name='megacap')


prices[close>5 & volume>100000 & 
         (volume>=max_volume & avg_delta_short<.98) & 
         (log(vp_order)-log(cap_order))>.35 &
         date==max(date, na.rm=T)][
           order(avg_delta_short)]%>%
  head(5)%>%
  dplyr::mutate( action='BUY',
                 order_type='MKT',
                 time_in_force='OPG') %>%
  write_strat(strat_name='volumelong')
