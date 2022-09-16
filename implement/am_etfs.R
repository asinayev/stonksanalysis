args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)

splits = 16

stocklist = stocklist_from_polygon(key = POLYKEY, date = Sys.Date()-1, 
                                   financials=F, cores=splits, ticker_type='ETF')

prices = stocklist$ticker %>%
  parallel::mclapply(
    stock_history,
    start_date = Sys.Date()-3*365, end_date = Sys.Date()+2, key = POLYKEY, 
    print=F, check_ticker=F,mc.cores = splits) %>% 
  rbindlist(use.names=TRUE, fill=T)

prices = prices[, .SD[1], by=.(stock, Date)][
  ,.(symbol=stock,date=Date, AdjClose, open, high, low, volume, close=AdjClose)] %>%
  merge(stocklist[,.(symbol=ticker, name)], all.x=T)
setorder(prices, symbol, date)

lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)
rally_avg(prices,200)

prices[date==max(date, na.rm=T) & volume>75000 & close>7 & 
         !grepl('short|bear|inverse', name, ignore.case = T) &
         close<lag1high & sell_rally_day>2 & 
         ((sell_rally_avg-avg_delta)/sell_rally_avg)>.018,
       .(date, symbol, close, volume)] %>%
  dplyr::mutate( action='BUY', order_type='MKT', time_in_force='OPG') %>%
  write_strat(strat_name='rally_etfs')

prices[order(RSI,decreasing=F)][
  date==max(date, na.rm=T) & volume>75000 & close>7 & 
         (((close-low)/(high-low))<.05 ) & 
         ((high/close) > 1.075 |
            ((running_low == low | RSI<.6) & ((avg_range/close) > .05)
            ) 
         )
       ,
       .(date, symbol, close, volume)]%>%
  head(5) %>%
  dplyr::mutate( action='BUY', order_type='MKT', time_in_force='OPG') %>%
  write_strat(strat_name='revert_etfs')

prices[order(lagging_corr_long, decreasing = F)][
  date==max(date, na.rm=T) & volume>250000 & close>7 &
    avg_delta_short<.975 & lagging_corr_long> .35,
  .(date, symbol, close, volume)] %>%
  head(5) %>%
  dplyr::mutate( action='BUY', order_type='MKT', time_in_force='OPG') %>%
  write_strat(strat_name='corr_long_etfs')
