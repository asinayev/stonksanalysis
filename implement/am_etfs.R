args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)

sell_rally_avg = function(price_dat){
  days=nrow(price_dat)
  return( mean(ifelse(price_dat[,1]>price_dat[days,5], 
                      price_dat[days,2], price_dat[,4])/price_dat[,3]))
}

sell_rally_window=200
delta_window=25
splits = 16

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
setorder(prices, symbol, date)

prices[,c("lag1close", "lag2close", "lead1close"):=shift(close, n = c(1:2,-1), type = "lag"),symbol]
prices[,sell_rally_increment:=ifelse(lag1close <  shift(high,n = 2, type="lag") | 
                                       is.na(shift(high,n = 2, type="lag")), 
                                     0, 1),symbol]
prices[,sell_rally_increment:=cumsum(sell_rally_increment), symbol]
prices[,sell_rally:=close[.N], .(sell_rally_increment,symbol)]
prices[,sell_rally_date:=date[.N], .(sell_rally_increment,symbol)]
prices[,sell_rally_day:=rowid(sell_rally_increment,symbol)]

prices[symbol %in% prices[,.N,symbol][N>sell_rally_window,symbol],
       sell_rally_avg:= zoo::rollapply(data=.SD[,.(sell_rally_date=as.integer(sell_rally_date),
                                                   close,open,sell_rally,
                                                   date=as.integer(date))],
                                       FUN=sell_rally_avg,
                                       width=sell_rally_window, align='right',by.column = FALSE,fill=NA
       ),symbol ]



prices[symbol %in% prices[,.N,symbol][N>delta_window,symbol],
       delta_avg:= SMA(close/lag1close, n = delta_window ),symbol ]
prices[symbol %in% prices[,.N,symbol][N>delta_window,symbol]
       ,running_low:= zoo::rollapply(low,min,width=delta_window, align='right',fill=NA),symbol ]

prices[date==max(date, na.rm=T) & volume>100000 & close>5 & 
         high<lag1close & 
         sell_rally_day>2 & (sell_rally_avg-delta_avg)>.02,
       .(date, symbol, close, volume)] %>%
  dplyr::mutate( action='BUY', order_type='MKT', time_in_force='OPG') %>%
  write_strat(strat_name='rally_etfs')

prices[date==max(date, na.rm=T) & volume>100000 & close>5 & 
         sell_rally_day>5 & running_low==low & ((close-low)/(high-low))<.025 & high/low>1.025,
       .(date, symbol, close, volume)] %>%
  dplyr::mutate( action='BUY', order_type='MKT', time_in_force='OPG') %>%
  write_strat(strat_name='revert_etfs')