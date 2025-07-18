args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)
prices = fread('/tmp/prices.csv')
snapshot=fread('/tmp/current_moves.csv')

setorder(prices, symbol, date)
prices = prices[!is.na(close), tail(.SD,150), by=symbol]
lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)

trending=snapshot[((as.integer(Sys.time())-as.integer(updated))/60/60)<1 & overnight_delta>1.0175]

just_news = get_prev_day_news(Sys.Date(),key=POLYKEY,full_prevday = F, apply_=T)

if(!is.null(just_news)){
  
  news_moves = just_news %>%
    clean_news %>%
    merge(prices, by=c('date','symbol'), all.x=T)
    news_moves[grepl('(dividend|repurchase|buyback|outlook|guidance|public offer|strategic)', title, ignore.case = T) &
                 avg_delta_short<1 & symbol %in% trending$symbol &
                !is.na(single_ticker) &
                avg_volume>100000 & volume>100000 & close>6  ] %>%
    dplyr::group_by(symbol) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::arrange(day_drop_norm/sd_from0) %>%
    head(1)  %>%
    dplyr::mutate( action='BUY', 
                   order_type='MKT',
                   time_in_force='OPG') %>%
    data.table %>%
    write_strat(strat_name='div_news')
  
  news_moves[symbol %in% trending$symbol &
               publisher.name %in% c('The Motley Fool','GlobeNewswire Inc.') &
               !is.na(single_ticker) &
               avg_volume>100000 & volume>100000 & close>6 ] %>%
    dplyr::group_by(symbol) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::arrange(close) %>%
    head(1)  %>%
    dplyr::mutate( action='BUY', 
                   order_type='MKT',
                   time_in_force='OPG') %>%
    data.table %>%
    write_strat(strat_name='news_trend')
  
  news_moves[avg_delta_short<.99 & 
               grepl('(public offer|quarter)', title, ignore.case = T)  &  
               !is.na(single_ticker) &
               avg_volume>100000 & volume>100000 & close>6 ] %>%
    dplyr::group_by(symbol) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::arrange(day_drop_norm/sd_from0) %>%
    head(1)  %>%
    dplyr::mutate( action='BUY', 
                   order_type='MKT',
                   time_in_force='OPG') %>%
    data.table %>%
    write_strat(strat_name='news_revert')
  
  news_moves%>%
    dplyr::group_by(symbol) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    data.table %>%
    fwrite('/tmp/stonksanalysis/all_news.csv')
} else { print("No news since yesterday") }
