require(tidyquant, quietly = T)
require(data.table, quietly = T)

args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){args='~/stonksanalysis'}
setwd(args[1])
source("polygon.R", local=T)
POLYKEY = Sys.getenv('POLYGONKEY')

stopifnot(POLYKEY!='')

current_moves = "https://api.polygon.io/v2/snapshot/locale/us/markets/stocks/tickers?apiKey=%s" %>%
  sprintf(POLYKEY) %>%
  hit_polygon

current_moves$tickers$ticker %>%
  parallel::mclapply(
    stock_history,
    start_date = Sys.Date()-365, end_date = Sys.Date()+1, key = POLYKEY, 
    print=F, check_ticker=T,mc.cores = 16) %>% 
  rbindlist(use.names=TRUE, fill=T)

  