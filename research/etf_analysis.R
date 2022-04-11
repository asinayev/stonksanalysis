setDTthreads(threads = 4)

prices[,sell_rally_increment:=ifelse(shift(close,n=1,type='lag')<shift(high,n = 2, type="lag") | is.na(shift(high,n = 2, type="lag")), 0, 1),symbol]
prices[,sell_rally_increment:=cumsum(sell_rally_increment), symbol]
prices[,sell_rally:=close[.N], .(sell_rally_increment,symbol)]
prices[,sell_rally_date:=date[.N], .(sell_rally_increment,symbol)]
prices[,sell_rally_increment:=NULL]


sell_rally_avg = function(price_dat){
  days=nrow(price_dat)
  return( mean(ifelse(price_dat[,1]>price_dat[days,5], 
                      price_dat[days,2], price_dat[,4])/price_dat[,3]))
}
prices[,sell_rally_avg:=NULL]
prices[,delta_avg:=NULL]
window=400
system.time(
  prices[symbol %in% prices[,.N,symbol][N>window,symbol]
         ,
         sell_rally_avg:= zoo::rollapply(data=.SD[,.(sell_rally_date=as.integer(sell_rally_date),
                                                     close,
                                                     open,
                                                     sell_rally,
                                                     date=as.integer(date))],
                                         FUN=sell_rally_avg,
                                         width=window, align='right',by.column = FALSE,fill=NA
         ),symbol ]
)

#400 / 25 /.025
# year       V1    N  V3             V4
# 1: 2018 1.049323   24   5 0.9583333 days
# 2: 2019 1.016563  305  13 2.9508197 days
# 3: 2020 1.017770 1113 116 3.1725067 days
# 4: 2021 1.030880  351  22 3.0655271 days
# 5: 2022 1.039534  193  19 2.2435233 days

prices[symbol %in% prices[,.N,symbol][N>window,symbol],
       delta_avg:= SMA(shift(close,1,type='lag')/shift(close,2,type='lag'), n = 25 ),symbol ]
prices[volume>100000 & close>10 & wday(date)<6 & sell_rally/open<2 & (sell_rally_avg-delta_avg)>.025,
       .(mean(sell_rally/open),.N,length(unique(symbol)),mean(sell_rally_date-date)),
       year(date)][order(year)]




prices[symbol %in% prices[,.N,symbol][N>30,symbol]
       ,day30low:= zoo::rollapply(low,min,width=30, align='right',fill=NA),symbol ]
prices[,lead1sellrally:= shift(sell_rally,1,type='lead'),symbol ]

prices[volume>100000 & close>10 & 
         day30low==low & ((close-low)/(high-low))<.025 & lead1sellrally/lead1open<2 & ((high/low) > 1.025)
       , .( mean(lead1sellrally/lead1open,na.rm=T),sd(lead1sellrally/lead1open,na.rm=T),length(unique(symbol)), length(unique(date)),.N)
       , year(date)]

# The strategy of buying after the high is broken still needs work -- try only cases where it has worked?
# prices[symbol %in% prices[,.N,symbol][N>5,symbol]
#        ,day4high:= zoo::rollapply(high,max,width=4, align='right',fill=NA),symbol ]
# 
# 
# prices[volume>100000 & close>10 & #wday(date)<4 & 
#          lead1sellrally/close<2 & 
#          day5high==high & ((close-low)/(high-low)) %between% c(.01,.5) 
#          & (high/low) > 1.05
#        , .(mean(lead1sellrally/lead1open,na.rm=T),sd(lead1sellrally/close,na.rm=T),length(unique(symbol)), length(unique(date)),.N)
#        , year(date)]
