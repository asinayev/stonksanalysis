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

fwrite(current_moves_dt[ticker=='SPY'], '/tmp/spy_change.csv') #has todaysChangePerc
fwrite(current_moves_dt[,.(symbol=ticker, open=prevDay.o, low=prevDay.l, high=prevDay.h, close=prevDay.c, volume=prevDay.v)], '/tmp/lastday_prices.csv')

prices = fread('/tmp/prices.csv')

current_moves_dt[todaysChangePerc>15 & min.av>10000 & min.c>5 & ticker %in% prices$symbol] %>%
  dplyr::mutate( symbol=ticker, action='SELL', 
                 strike_price=trunc(prevDay.c*1150,3)/1000, 
                 order_type='LMT', time_in_force='OPG') %>%
  write_strat(strat_name='nightbot')
