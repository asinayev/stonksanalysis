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
