require(tidyquant, quietly = T)
require(data.table, quietly = T)

args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){args='~/stonksanalysis'}
setwd(args[1])
source("polygon.R", local=T)
POLYKEY = Sys.getenv('POLYGONKEY')

stopifnot(POLYKEY!='')

prices = fread('/tmp/prices.csv')
setorder(prices, symbol, date)
prices = prices[!is.na(close), tail(.SD,126), by=symbol]
lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)

just_news = get_newsday(Sys.Date(),key=POLYKEY,yesterday_news=F, apply_=T)

news_moves = just_news %>%
  clean_news %>%
  merge(prices, by=c('date','symbol'), all.x=T)

# long stocks that fell before revenue
current_news[grepl('earning', title, ignore.case = T) &
             grepl('revenue', title, ignore.case = T) &
             publisher.name=='Zacks Investment Research' &
             avg_volume>50000 & volume>50000 & close>5  & 
             close/open <.99] %>%
 dplyr::mutate( action='BUY', 
                order_type='MKT',
                time_in_force='OPG') %>%
 write_strat(strat_name='zacks_earn')