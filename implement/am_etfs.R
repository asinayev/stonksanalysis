args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)

splits = 16

stocklist = stocklist_from_polygon(key = POLYKEY, date = Sys.Date()-1, 
                                   financials=F, cores=splits, ticker_type='ETF') %>%
  rbind(stocklist_from_polygon(key = POLYKEY, date = Sys.Date()-1, 
                               financials=F, cores=splits, ticker_type='ETV'))

prices = stocklist$ticker %>%
  parallel::mclapply(
    stock_history,
    start_date = Sys.Date()-3*365, end_date = Sys.Date()+2, key = POLYKEY, 
    print=F, check_ticker=F,mc.cores = splits) %>% 
  rbindlist(use.names=TRUE, fill=T)

prices = prices[, .SD[1], by=.(stock, Date)][
  ,.(symbol=stock,date=Date, AdjClose, open, high, low, volume, close=AdjClose)] %>%
  merge(stocklist[,.(symbol=ticker, name)], all.x=T)

prices = only_passing(prices, min_volume=75000, min_close=7, last_n = F)

lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)
rally_avg(prices,200)
#prices=key_etfs(prices, low_corr_thresh=.33)

prices[,short:=grepl('short|bear|inverse', name, ignore.case = T)]
prices[,lever:=grepl('2x|3x|leverag|ultra', name, ignore.case = T)]

prices[order(lever, avg_volume,decreasing = F)][
  date==max(date, na.rm=T) & volume>500000 & close>7 & 
         close<lag1high & sell_rally_day>2 & 
         ((sell_rally_avg-avg_delta)/sell_rally_avg)>.018,
       .(date, symbol, close, volume)] %>%
  dplyr::mutate( action='BUY', order_type='MKT', time_in_force='OPG') %>%
  head(3) %>%
  write_strat(strat_name='rally_etfs')

prices[order(lever, avg_volume,decreasing=F)][
  date==max(date, na.rm=T) & avg_volume>1000000 & close>7 &  
         (((close-low)/avg_range)<.2 ) & 
         ((high/close) > 1.075 | avg_delta<.99 |
            (!short & lever &  (MACD_slow<.975 | running_low == low ) ) 
          ),
       .(date, symbol, close, volume)]%>%
  head(3) %>%
  dplyr::mutate( action='BUY', order_type='MKT', time_in_force='OPG') %>%
  write_strat(strat_name='revert_etfs')

prices[order(lever, avg_volume, decreasing = F)][
  date==max(date, na.rm=T) & 
    avg_volume>500000 & close>7 & 
    avg_delta_short<.975 & lagging_corr_long> .35,
  .(date, symbol, close, volume)] %>%
  head(3) %>%
  dplyr::mutate( action='BUY', order_type='MKT', time_in_force='OPG') %>%
  write_strat(strat_name='corr_long_etfs')
