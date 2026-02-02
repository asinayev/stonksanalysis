args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)

etf_list = fread("other_datasources/etf_list.csv")

splits = 16

stocklist = stocklist_from_polygon(key = POLYKEY, date = Sys.Date()-1, 
                                   cores=splits, ticker_type='ETF') %>%
  rbind(stocklist_from_polygon(key = POLYKEY, date = Sys.Date()-1, 
                               cores=splits, ticker_type='ETV')) %>%
  merge(etf_list[,.(ticker,category,leverage)], by='ticker', all.x=T)

prices = stocklist$ticker %>%
  parallel::mclapply(
    stock_history,
    start_date = Sys.Date()-3*365, end_date = Sys.Date()+2, key = POLYKEY, 
    check_ticker=F,mc.cores = splits) %>% 
  rbindlist(use.names=TRUE, fill=T)

prices = prices[, .SD[1], by=.(stock, date)][
  ,.(symbol=stock, date, open, high, low, volume, close, market_cap=NA)] %>%
  merge(stocklist[,.(symbol=ticker, name, category, leverage)], all.x=T)

prices = only_passing(prices, min_volume=75000, min_close=7, last_n = F)

lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)
#rally_avg(prices,200)
#prices=key_etfs(prices, low_corr_thresh=.33)

prices[order(day_drop_norm/sd_from0, decreasing=F)][
  date==max(date, na.rm=T) & volume>1000000 & close>7 & category %in% c('equity basket') &
         close<lag1high & sell_rally_day>10 & sd_from0>.015 ] %>%
  dplyr::mutate( action='BUY', 
                 order_type='Adaptive',
                 time_in_force='DAY') %>%
  head(1) %>%
  write_strat(strat_name='rally_etfs')

prices[order(day_drop_norm/sd_from0, decreasing=F)][
  date==max(date, na.rm=T) & volume>1000000 & close>7 & 
    category %in% c('equity basket','physical commodities') &
    ((high-close)/avg_range > 1)  & 
    (avg_delta< (1-.005*abs(leverage)) ) ]%>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='Adaptive',
                 time_in_force='DAY') %>%
  write_strat(strat_name='revert_etfs')

prices[order(day_drop_norm/sd_from0, decreasing=F)][
  date==max(date, na.rm=T) & 
    volume>1000000 & close>7 & category %in% c('equity basket','physical commodities') &
    avg_delta_short<.975 & lagging_corr_long> .7] %>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='Adaptive',
                 time_in_force='DAY') %>%
  write_strat(strat_name='corr_long_etfs')

prices[order(day_drop_norm/sd_from0, decreasing=F)][
  date==max(date, na.rm=T) & 
    volume>1000000 & close>7 & 
    category %in% c('equity basket','physical commodities') &
    (((close-low)/(high-low))<.15 ) & (MACD/MACD_slow)< .98 ]%>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='Adaptive',
                 time_in_force='DAY') %>%
  write_strat(strat_name='drop_etfs')

prices[order(sd_from0, decreasing=T)][
  date==max(date, na.rm=T) & 
    volume>500000 & close>7 & category=='equity basket' &
    ((lag1close-close) > avg_range*.25)  ]%>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='Adaptive',
                 time_in_force='DAY') %>%
  write_strat(strat_name='deviant_etfs')

all_matching_pairs=matching_pairs_for_year(year(max(prices$date)), 
                                           dataset=prices[category %in% c('equity basket', 'physical commodities')], 
                                           reference_etfs=reference_etfs)
all_matching_pairs[order(day_drop_norm/sd_from0, decreasing=F)][
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

prices[order(day_drop_norm/sd_from0, decreasing=F)][
  date==max(date, na.rm=T) & symbol%in%c('TNA','UPRO','YINN') &
         volume>100000 & close>7 &
         close>lag1close*1.03]%>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='Adaptive',
                 time_in_force='DAY') %>%
  write_strat(strat_name='tru_rally')

prices[order(day_drop_norm/sd_from0, decreasing=F)][
  date==max(date, na.rm=T) & 
    category %in% c('equity basket', 'physical commodities') &
    volume>1000000 & close>7 & leverage>0 &
    avg_root_delta< -.03 & root_delta_corr > .15]%>%
  head(1) %>%
  dplyr::mutate( action='BUY', 
                 order_type='Adaptive',
                 time_in_force='DAY') %>%
  write_strat(strat_name='corr_reverse')

prices[date==max(date, na.rm=T) ] %>%
  fwrite('/tmp/stonksanalysis/all_etfs.csv')
