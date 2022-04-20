setDTthreads(threads = 4)

setorder(prices, symbol, date)
prices[,sell_rally_increment:=ifelse(lag1close<shift(high,n = 2, type="lag") | is.na(shift(high,n = 2, type="lag")), 0, 1),symbol]
prices[,sell_rally_increment:=cumsum(sell_rally_increment), symbol]
prices[,sell_rally:=close[.N], .(sell_rally_increment,symbol)]
prices[,sell_rally_date:=date[.N], .(sell_rally_increment,symbol)]
prices[,sell_rally_day:=rowid(sell_rally_increment,symbol)]
prices[,sell_rally_increment:=NULL]


sell_rally_avg = function(price_dat){
  days=nrow(price_dat)
  return( mean(ifelse(price_dat[,1]>price_dat[days,5], 
                      price_dat[days,2], price_dat[,4])/price_dat[,3]))
}

sell_rally_window=200
delta_window=25

prices[,sell_rally_avg:=NULL]
system.time({
  setorder(prices, symbol, date)
  prices[symbol %in% prices[,.N,symbol][N>sell_rally_window,symbol]
         ,
         sell_rally_avg:= zoo::rollapply(data=.SD[,.(sell_rally_date=as.integer(sell_rally_date),
                                                     close,
                                                     open,
                                                     sell_rally,
                                                     date=as.integer(date))],
                                         FUN=sell_rally_avg,
                                         width=sell_rally_window, align='right',by.column = FALSE,fill=NA
         ),symbol ]
}
)

#200 / 25 /.02
# year       V1   N V3
# 1: 2018 1.019552 136 15
# 2: 2019 1.015400 210 17
# 3: 2020 1.018274 647 82
# 4: 2021 1.025881 606 40
# 5: 2022 1.057030 100 15

# Rally ETFs
setorder(prices, symbol, date)
prices[,lead1sellrally:= shift(sell_rally,1,type='lead'),symbol ]

prices[,delta_avg:=NULL]
prices[symbol %in% prices[,.N,symbol][N>delta_window,symbol],
       delta_avg:= SMA(close/lag1close, n = delta_window ),symbol ]
prices[volume>100000 & close>5 &
         date!=sell_rally_date & 
         sell_rally_day>2 &
         lead1sellrally/lead1open<2 & (sell_rally_avg-delta_avg)>.02 ,
       .(mean(lead1sellrally/lead1open),.N,length(unique(symbol))),
       year(date)][order(year)]


# revert ETFs
prices[symbol %in% prices[,.N,symbol][N>delta_window,symbol]
       ,running_low:= zoo::rollapply(low,min,width=delta_window, align='right',fill=NA),symbol ]

prices[volume>100000 & close>10 & sell_rally_day>5 & 
         running_low==low & ((close-low)/(high-low))<.025 & lead1sellrally/lead1open<2 & ((high/low) > 1.025)
       , .( mean(lead1sellrally/lead1open,na.rm=T),sd(lead1sellrally/lead1open,na.rm=T),length(unique(symbol)), length(unique(date)),.N,mean(sell_rally_date-date))
       , year(date)][order(year)]


# Low Close ETFs
prices[volume>100000 & close>5 & lead1sellrally/close<2 & 
         ((close-low)/(high-low)) < .05
         & (high/close) > 1.05
       , .(mean(lead1sellrally/close,na.rm=T),sd(lead1sellrally/close,na.rm=T),length(unique(symbol)), length(unique(date)),.N,mean(sell_rally_date-date))
       , year(date)][order(year)]
