args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)


current_moves = "https://api.polygon.io/v2/snapshot/locale/us/markets/stocks/tickers?apiKey=%s" %>%
  sprintf(POLYKEY) %>%
  hit_polygon
current_moves_dt = data.table(current_moves$tickers)

current_moves_dt[todaysChangePerc>15 & min.av>10000 & min.c>5] %>%
  dplyr::mutate( symbol=ticker, action='SELL', 
                 strike_price=trunc(prevDay.c*1150,3)/1000, 
                 order_type='LMT', time_in_force='OPG') %>%
  write_strat(strat_name='nightbot')
