require(tidyquant, quietly = T)
require(data.table, quietly = T)

args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){args='~/stonksanalysis'}
setwd(args[1])
source("polygon.R", local=T)
POLYKEY = Sys.getenv('POLYGONKEY')

stopifnot(POLYKEY!='')

splits = 16

sell_rally_avg = function(price_dat){
  days=nrow(price_dat)
  return( mean(ifelse(price_dat[,1]>price_dat[days,5], 
                      price_dat[days,2], price_dat[,4])/price_dat[,3]))
}


stocklist = stocklist_from_polygon(key = POLYKEY, date = paste(year(Sys.Date()),'01','01', sep='-'), 
                                   financials=F, cores=splits, ticker_type='ETF')

prices = stocklist$ticker %>%
  parallel::mclapply(
    stock_history,
    start_date = Sys.Date()-3*365, end_date = Sys.Date()+2, key = POLYKEY, 
    print=F, check_ticker=F,mc.cores = splits) %>% 
  rbindlist(use.names=TRUE, fill=T)

prices = prices[, .SD[1], by=.(stock, Date)][
  ,.(symbol=stock,date=Date, AdjClose, open, high, low, volume, close=AdjClose)]

prices[,sell_rally_increment:=ifelse(shift(close,n=1,type='lag') <  shift(high,n = 2, type="lag") | 
                                       is.na(shift(high,n = 2, type="lag")), 
                                     0, 1),symbol]
prices[,sell_rally_increment:=cumsum(sell_rally_increment), symbol]
prices[,sell_rally:=close[.N], .(sell_rally_increment,symbol)]
prices[,sell_rally_date:=date[.N], .(sell_rally_increment,symbol)]

window=400
prices[symbol %in% prices[,.N,symbol][N>window,symbol],
       sell_rally_avg:= zoo::rollapply(data=.SD[,.(sell_rally_date=as.integer(sell_rally_date),
                                                   close,open,sell_rally,
                                                   date=as.integer(date))],
                                       FUN=sell_rally_avg,
                                       width=window, align='right',by.column = FALSE,fill=NA
       ),symbol ]



prices[symbol %in% prices[,.N,symbol][N>window,symbol],
       delta_avg:= SMA(shift(close,1,type='lag')/shift(close,2,type='lag'), n = 25 ),symbol ]

prices[date==max(date, na.rm=T) & (sell_rally_avg-delta_avg)>.025,
       .(date, symbol, close, volume)] %>%
  dplyr::mutate( action='BUY', order_type='MKT', time_in_force='OPG') %>%
  fwrite('/tmp/rally_etfs.csv')
