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



prices[,sell_rally :=ifelse(close>shift(high,n = 1, type="lag"), close, NA),symbol]
prices[,sell_rally_increment:=cumsum(!is.na(shift(sell_rally,1,type='lag'))), symbol]
prices[,sell_rally:=sell_rally[.N], .(sell_rally_increment,symbol)]
prices[,sell_rally_date:=date[.N], .(sell_rally_increment,symbol)]
prices[,sell_rally_increment:=NULL]

prices[symbol %in% prices[days_around>300, unique(symbol)] & !is.na(sell_rally),
       sell_rally_avg:= SMA(shift(sell_rally/open,25,type='lag'), n = 200 ),symbol ]
prices[symbol %in% prices[days_around>300, unique(symbol)] & !is.na(sell_rally),
       delta_avg:= SMA(shift(close,1,type='lag')/shift(close,2,type='lag'), n = 25 ),symbol ]
prices[sell_rally/open<2 & (sell_rally_avg-delta_avg)%between%c(.02,.04),
       .(mean(sell_rally/open),.N,length(unique(symbol)),mean(sell_rally_date-date)),
       year(date)][order(year)]

# sell_rally_avg = function(price_dat){
#   price_dat=data.table(price_dat)
#   price_dat[sell_rally_date>date[.N], sell_rally:=close[.N]]
#   
#   return( price_dat[,mean(sell_rally/open)])
# }
# system.time(
# prices[symbol %in% prices[,.N,symbol][N>window,symbol][1:20],
#        sell_rally_avg:= zoo::rollapply(data=.SD[,.(sell_rally_delta=sell_rally/open,
#                                                    sell_rally_date=as.integer(sell_rally_date),close,open,
#                                                    date=as.integer(date))],
#                                        FUN=sell_rally_avg,
#                                        width=window, align='right',by.column = FALSE,fill=NA
#        ),symbol ]
# )



