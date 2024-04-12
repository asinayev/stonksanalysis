args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)
prices = fread('/tmp/prices.csv')

setorder(prices, symbol, date)
prices = prices[!is.na(close), tail(.SD,126), by=symbol]
lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)

just_news = get_prev_day_news(Sys.Date(),key=POLYKEY,full_prevday = F, apply_=T)

news_moves = just_news %>%
  clean_news %>%
  merge(prices, by=c('date','symbol'), all.x=T)

# long stocks that fell before revenue
news_moves[grepl('earning', title, ignore.case = T) &
           grepl('revenue', title, ignore.case = T) &
           publisher.name=='Zacks Investment Research' &
           avg_volume>50000 & volume>50000 & close>5  & 
           open-close > avg_range/3 ] %>%
  dplyr::group_by(symbol) %>%
  dplyr::filter(dplyr::row_number()==1) %>%
  dplyr::arrange(avg_delta) %>%
  head(3)  %>%
  dplyr::mutate(action='BUY', 
                order_type=ifelse(volume>250000,'MKT','MIDPRICE'),
                time_in_force='DAY') %>%
  data.table %>%
  write_strat(strat_name='zacks_earn')


news_moves[(grepl('(new|announce|declare|authori|start).*(repurchase|buyback)', title, ignore.case = T)|
                (grepl('(dividend|repurchase|buyback)', title, ignore.case = T) & avg_delta>1 & avg_delta_short<1)) &
           !is.na(single_ticker) &
           avg_volume>50000 & volume>50000 & close>5  & 
           market_cap %between% c(0.5*10^9, 10*10^9) ] %>%
  dplyr::group_by(symbol) %>%
  dplyr::filter(dplyr::row_number()==1) %>%
  dplyr::arrange(avg_delta_short) %>%
  head(3)  %>%
  dplyr::mutate(action='BUY', 
                order_type=ifelse(volume>250000,'MKT','MIDPRICE'),,
                time_in_force='DAY') %>%
  data.table %>%
  write_strat(strat_name='div_news')

news_moves%>%
  dplyr::group_by(symbol) %>%
  dplyr::filter(dplyr::row_number()==1) %>%
  data.table %>%
  fwrite('/tmp/stonksanalysis/all_news.csv')
  
