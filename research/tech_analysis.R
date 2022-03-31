setDTthreads(threads = 4)
get_tech = function(price_dat){
  low_order = order(price_dat$low)
  high_order = order(price_dat$high, decreasing = T)
  list(low_dates=price_dat$date[low_order],
       high_dates=price_dat$date[high_order], 
       low_vals=price_dat$low[low_order], 
       high_vals=price_dat$high[high_order])
}

# boundaries are measured in units of (high-low) from low
dbl_bottom = function(price_dat, 
                      recent_low_days_ago=2, 
                      recent_low_within=c(-.1,.1), 
                      interlow_high=c(.75,2)
                      ){
  price_dat=data.table(price_dat)
  techs = get_tech(price_dat[1:(.N-recent_low_days_ago-1)])
  recent_low_i = nrow(price_dat)-recent_low_days_ago
  price_range = techs$high_vals[1]-techs$low_vals[1]
  return( c(
       price_dat[recent_low_i,low] #recent low
     , techs$low_vals[1] # lowest low
     , techs$high_vals[1] # highest high
     , price_dat[date %between% c(techs$low_dates[1], date[recent_low_i-1]),max(high)] #interlow_high
     , price_dat[.N,date] - techs$low_dates[1] #lowest_low_days_ago
   ))
}

day_interval=c(1500,100)
window = 45

prices[, c('is_reversal','recent_low','prev_low','lowest_low_days_ago'):=NULL]
prices[ date %between% (Sys.Date()-day_interval) & 
          symbol%in%prices[date %between% (Sys.Date()-day_interval),
                           .(.N, avg_dol_vol = mean(volume*close,na.rm=T) ),
                           symbol][N>window & avg_dol_vol>10000000,symbol], 
        c('recent_low','lowest_low','highest_high','interlow_high','lowest_low_days_ago'):=
         zoo::rollapply(data=.SD[,.(high,low,close,date=as.integer(date))],
                        FUN=dbl_bottom,
                        width=window, align='right',by.column = FALSE,fill=NA,
                        recent_low_days_ago=0
                        ) %>% matrix(ncol=5) %>% data.frame,
                symbol]


#This seems to work for stocks
prices[!is.na(lead1nightdelta*lead1daydelta)  &
         lowest_low_days_ago>35 & 
         interlow_high>lowest_low*1.2 &
         close>recent_low*1.1 &
         close>(recent_low+(highest_high-lowest_low)*.4) &
         recent_low > lowest_low +(highest_high-lowest_low)*.1 #&
       #recent_low < lowest_low+(highest_high-lowest_low)*.1
       ,
       .(mean(lead1nightdelta),mean(lead1daydelta),.N ),
       .(year(date))]



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
                                                     close,open,sell_rally,
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
prices[sell_rally/open<2 & (sell_rally_avg-delta_avg)>.025,
       .(mean(sell_rally/open),.N,length(unique(symbol)),mean(sell_rally_date-date)),
       year(date)][order(year)]
