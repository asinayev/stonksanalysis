args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)

splits = 16

stocklist = stocklist_from_polygon(key = POLYKEY, date = Sys.Date()-1, 
                                   cores=splits, ticker_type='ETF') %>%
  rbind(stocklist_from_polygon(key = POLYKEY, date = Sys.Date()-1, 
                               cores=splits, ticker_type='ETV'))

prices = stocklist$ticker %>%
  parallel::mclapply(
    stock_history,
    start_date = Sys.Date()-3*365, end_date = Sys.Date()+2, key = POLYKEY, 
    print=F, check_ticker=F,mc.cores = splits) %>% 
  rbindlist(use.names=TRUE, fill=T)

prices = prices[, .SD[1], by=.(stock, Date)][
  ,.(symbol=stock,date=Date, AdjClose, open, high, low, volume, close=AdjClose, market_cap=NA)] %>%
  merge(stocklist[,.(symbol=ticker, name)], all.x=T)

prices = only_passing(prices, min_volume=75000, min_close=7, last_n = F)

lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)
#rally_avg(prices,200)
#prices=key_etfs(prices, low_corr_thresh=.33)

prices[,short:=grepl('bear|inverse', name, ignore.case = T) | (grepl('short', name, ignore.case = T) & !grepl('term|duration|matur|long|income', name, ignore.case = T))]
prices[,lever:=grepl('2x|3x|leverag|ultra', name, ignore.case = T)]

prices[order(day_drop_norm, decreasing=F)][
  date==max(date, na.rm=T) & volume>1000000 & close>7 & 
         close<lag1high & sell_rally_day>10] %>%
  dplyr::mutate( action='BUY', 
                 order_type='Adaptive',
                 time_in_force='DAY') %>%
  head(1) %>%
  write_strat(strat_name='rally_etfs')

prices[order(day_drop_norm, decreasing=F)][
  date==max(date, na.rm=T) & volume>1000000 & close>7 &  
         (((close-low)/avg_range)<.15 ) & 
         ((high/close) > avg_range*2 | avg_delta<(.99-(.01*lever)))]%>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='Adaptive',
                 time_in_force='DAY') %>%
  write_strat(strat_name='revert_etfs')

prices[order(day_drop_norm, decreasing=F)][
  date==max(date, na.rm=T) & 
    volume>1000000 & close>7 & 
    avg_delta_short<.975 & lagging_corr_long> .35] %>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='Adaptive',
                 time_in_force='DAY') %>%
  write_strat(strat_name='corr_long_etfs')

prices[order(day_drop_norm, decreasing=F)][
  date==max(date, na.rm=T) & 
    volume>1000000 & close>7 & 
    !short & (avg_delta_short < .99-avg_range/close/2 ) ]%>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='Adaptive',
                 time_in_force='DAY') %>%
  write_strat(strat_name='drop_etfs')

all_matching_pairs=matching_pairs_for_year(year(max(prices$date)), 
                                           dataset=prices, 
                                           reference_etfs=reference_etfs)
all_matching_pairs[order(day_drop_norm, decreasing=F)][
  date==max(date, na.rm=T) &
    volume>500000 & close>7 &
    abs(1-close/lag1close-(reference_delta-1)*round(mult.reference_delta_short))>.0075  &
    avg_delta_short<1 &
    rsq>.98 & abs(mult.reference_delta_short-round(mult.reference_delta_short))<.15]%>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='Adaptive',
                 time_in_force='DAY') %>%
  write_strat(strat_name='arb_etfs')

should_trade_volatility = prices[
  date==max(date, na.rm=T) & symbol=='SPY',
    ((high/low>1.015)|(abs(close/lag1close-1)>.01)|(abs(avg_delta_short-1)>.005))]

prices[date==max(date, na.rm=T) & symbol=='SVXY' & should_trade_volatility]%>%
  dplyr::mutate( action='BUY', 
                 order_type='Adaptive',
                 time_in_force='DAY') %>%
  write_strat(strat_name='short_vix')

prices[order(day_drop_norm, decreasing=F)][
  date==max(date, na.rm=T) & symbol%in%c('TNA','UPRO','YINN') &
         volume>100000 & close>7 &
         close>lag1close*1.03]%>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='Adaptive',
                 time_in_force='DAY') %>%
  write_strat(strat_name='tru_rally')

prices[date==max(date, na.rm=T) ] %>%
  fwrite('/tmp/stonksanalysis/all_etfs.csv')
