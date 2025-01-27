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
current_moves_dt = current_moves_dt[,
                                    .(symbol=ticker,date=Sys.Date()-1, AdjClose=prevDay.c, 
                                       open=prevDay.o, high=prevDay.h, low=prevDay.l, 
                                       volume=prevDay.v, close=prevDay.c, overnight_delta=1+todaysChangePerc/100,
                                      updated=as.integer(updated/10^9)
                                       )]
fwrite(current_moves_dt, '/tmp/current_moves.csv')
