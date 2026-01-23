args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)

splits = 16

stocklist = stocklist_from_polygon(key = POLYKEY, date = Sys.Date()-30, details=T, cores=splits)

prices = stocklist$ticker %>%
  parallel::mclapply(
    stock_history,
    start_date = Sys.Date()-5*365, end_date = Sys.Date()+2, key = POLYKEY, 
    check_ticker=F,mc.cores = splits) %>% 
  rbindlist(use.names=TRUE, fill=T)

prices = prices[, .SD[1], by=.(stock, date)][
  ,.(symbol=stock, date, open, high, low, volume, close)] %>%
  merge(stocklist,  by.x='symbol', by.y='ticker')
fwrite(prices,'/tmp/prices.csv')
