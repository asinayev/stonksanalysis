require(tidyquant, quietly = T)
require(data.table, quietly = T)

splits = 16

stocklist = stocklist_from_polygon(key = POLYKEY, date = paste(year(Sys.Date()),'01','01', sep='-'), financials=F, cores=splits)

prices = stocklist$ticker %>%
  parallel::mclapply(
    stock_history,
    start_date = Sys.Date()-5, end_date = Sys.Date()+1, key = POLYKEY, 
    print=F, check_ticker=F,mc.cores = splits) %>% 
  rbindlist(use.names=TRUE, fill=T)

prices = prices[, .SD[1], by=.(stock, Date)][
  ,.(symbol=stock,date=Date, AdjClose, open, high, low, volume, close=AdjClose)]
fwrite(prices,'/tmp/prices.csv')