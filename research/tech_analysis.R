setDTthreads(threads = 2)
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

day_interval=c(1000,100)
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
                        recent_low_days_ago=2
                        ) %>% matrix(ncol=5) %>% data.frame,
                symbol]

prices[!is.na(lead1close*lead7close)  &
         lowest_low_days_ago<35 & 
         interlow_high>lowest_low+(highest_high-lowest_low)*.3 & 
         close>(recent_low+(highest_high-lowest_low)*.3) &
         recent_low > lowest_low #&
         #recent_low < lowest_low+(highest_high-lowest_low)*.1
         ,
       .(mean(lead1open/close),mean(lead1close/lead1open),mean(lead7close/close,na.rm=T),.N ),
       .(year(date))]

prices[!is.na(lead1close*lead7close)  &
         lowest_low_days_ago>35 & 
         interlow_high>lowest_low+(highest_high-lowest_low)*.3 & 
         lag1close>(recent_low+(highest_high-lowest_low)*.3) &
         recent_low > lowest_low #&
       #recent_low < lowest_low+(highest_high-lowest_low)*.1
       ,
       .(mean(open/lag1close),mean(close/open),mean(lead7close/lag1close,na.rm=T),.N ),
       .(year(date))]

# recovery best in the 5-15% range
prices[!is.na(lead1close*lead7close) & date %between% (Sys.Date()-c(500,200)) &
         interlow_high>lowest_low+(highest_high-lowest_low)*.1 & 
         close>lowest_low+(highest_high-lowest_low)*.3 &
         recent_low > lowest_low-(highest_high-lowest_low)*.1 &
         recent_low < lowest_low+(highest_high-lowest_low)*.1,
       .(mean(lead1open/close),mean(lead1close/lead1open),mean(lead7close/close,na.rm=T),.N ),
       round( (close-recent_low)/(highest_high-lowest_low), 1) ][order(round)]
