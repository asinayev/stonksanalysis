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
#    year      avg         sd stocks days   N          held
# 1: 2018 1.018360 0.09269388     16   93 142 6.161972 days
# 2: 2019 1.014729 0.07136405     17  126 213 5.178404 days
# 3: 2020 1.016711 0.16010467     80  177 644 5.111801 days
# 4: 2021 1.026145 0.06041440     41  185 617 4.505673 days
# 5: 2022 1.057030 0.08113986     15   53 100 4.220000 days

# Rally ETFs
setorder(prices, symbol, date)
prices[,lead1sellrally:= shift(sell_rally,1,type='lead'),symbol ]
prices[,lead1sellrallydate:= shift(sell_rally_date,1,type='lead'),symbol ]

prices[,delta_avg:=NULL]
prices[symbol %in% prices[,.N,symbol][N>delta_window,symbol],
       delta_avg:= SMA(close/lag1close, n = delta_window ),symbol ]
prices[volume>100000 & close>5 &
         date!=sell_rally_date & 
         sell_rally_day>2 &
         lead1sellrally/lead1open<2 & (sell_rally_avg-delta_avg)>.02 
       , .(avg = mean(lead1sellrally/lead1open,na.rm=T),sd = sd(lead1sellrally/lead1open,na.rm=T),stocks = length(unique(symbol)), days = length(unique(date)),.N,held=mean(lead1sellrallydate-date))
       , year(date)][order(year)]


# revert ETFs
prices[symbol %in% prices[,.N,symbol][N>delta_window,symbol]
       ,running_low:= zoo::rollapply(low,min,width=delta_window, align='right',fill=NA),symbol ]

prices[volume>100000 & close>10 & sell_rally_day>5 & 
         running_low==low & ((close-low)/(high-low))<.025 & lead1sellrally/lead1open<2 & ((high/low) > 1.025)
       , .( mean(lead1sellrally/lead1open,na.rm=T),sd(lead1sellrally/lead1open,na.rm=T),length(unique(symbol)), length(unique(date)),.N,mean(sell_rally_date-date))
       , year(date)][order(year)]


# Low Close ETFs
#    year      avg         sd stocks days    N          held
# 1: 2016 1.019533 0.04217613      4   13   13 5.846154 days
# 2: 2017 1.019504 0.09806495     47   58  123 7.138211 days
# 3: 2018 1.013713 0.10998111    146  104  587 6.824532 days
# 4: 2019 1.021030 0.08597074     77  144  334 6.167665 days
# 5: 2020 1.029370 0.11947684    586  191 1808 5.091814 days
# 6: 2021 1.020209 0.07927844    187  165  683 6.222548 days
# 7: 2022 1.022090 0.08867949    161   62  573 5.619546 days
prices[volume>100000 & close>5 & lead1sellrally/close<2 & 
         ((close-low)/(high-low)) < .05
         & (high/close) > 1.05
       , .(avg = mean(lead1sellrally/close,na.rm=T),sd = sd(lead1sellrally/close,na.rm=T),stocks = length(unique(symbol)), days = length(unique(date)),.N,held=mean(sell_rally_date-date))
       , year(date)][order(year)]
