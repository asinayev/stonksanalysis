args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)

etflist = stocklist_from_polygon(key = POLYKEY, date = Sys.Date(), financials=F, cores=splits, ticker_type='ETF')
etflist = etflist[!grepl('short|bear|inverse', name, ignore.case = T)]
current_moves = "https://api.polygon.io/v2/snapshot/locale/us/markets/stocks/tickers?apiKey=%s" %>%
  sprintf(POLYKEY) %>%
  hit_polygon


etf_moves = data.table(current_moves$tickers)[ticker %in% etflist$ticker]

subset(etf_moves, 
       prevDay.v>100000 & prevDay.c>10 &
       day.c<day.h/1.04 & day.c<(day.l+.1*(day.h-day.l)),
       select=c('ticker','day.l','day.h','day.c')) %>%
  dplyr::mutate( symbol=ticker, action='BUY', 
                 strike_price=pmin(day.l + .04*(day.h-day.l), day.h/1.05), 
                 order_type='LOC', time_in_force='') %>%
  write_strat(strat_name='lowclose_etfs')
