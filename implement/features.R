lag_lead_roll = function(stock_dat, corr_window, roll_window, short_roll_window){
  setorder(stock_dat, symbol, date)
  stock_dat[,c("lag1close", "lag2close", "lead1close", "lead2close", "lead5close"):=shift(close, n = c(1,2,-1,-2,-5), type = "lag"),symbol]
  stock_dat[,c("lag1open",  "lag2open", "lead1open"):=shift(open,  n = c(1:2,-1), type = "lag"),symbol]
  stock_dat[,c("lag1high",  "lag2high", "lead1high" ):=shift(high,  n = c(1,2,-1), type = "lag"),symbol]
  stock_dat[,c("lag1low",   "lag2low"  ):=shift(low,   n = 1:2, type = "lag"),symbol]
  stock_dat[,c("lag1volume"  ):=shift(volume,   n = 1, type = "lag"),symbol]
  
  stock_dat[symbol %in% stock_dat[,.N,symbol][N>roll_window,symbol],
            avg_delta:= SMA(close/lag1close, n = roll_window ),symbol ]
  stock_dat[symbol %in% stock_dat[,.N,symbol][N>roll_window,symbol],
            avg_delta_short:= SMA(close/lag1close, n = short_roll_window ),symbol ]
  stock_dat[symbol %in% stock_dat[,.N,symbol][N>roll_window,symbol]
            ,running_low:= zoo::rollapply(low,min,width=roll_window, align='right',fill=NA),symbol ]
  stock_dat[symbol %in% stock_dat[,.N,symbol][N>roll_window,symbol]
            ,avg_range:= frollmean(high-low ,n = roll_window, align='right',fill=NA),symbol ]
  stock_dat[symbol %in% stock_dat[,.N,symbol][N>roll_window,symbol]
            ,avg_volume:= frollmean(volume ,n = roll_window, align='right',fill=NA),symbol ]
  stock_dat[symbol %in% stock_dat[,.N,symbol][N>(corr_window+short_roll_window), unique(symbol)],
            lagging_corr_long:=
              runCor( close/open, avg_delta_short, corr_window),
            symbol]
  stock_dat[symbol %in% stock_dat[,.N,symbol][N>roll_window,symbol]
            ,RSI:= frollmean(pmax(0, close-lag1close) ,n = roll_window, align='right',fill=NA)/
              frollmean(pmax(0, lag1close-close) ,n = roll_window, align='right',fill=NA),symbol ]
}

regression_features = function(stock_dat){
  prices[,day_delta:= close/open]
  prices[,day_fall:= low/open]
  prices[,day_rise:= high/open]
  prices[,night_delta:= open/lag1close]
  prices[,future_day_delta:= lead1close/lead1open]
}

rally = function(stock_dat,
                 sell_rule=function(dat){dat$close>=dat$lag1high},
                 varnames=c('sell_rally','sell_rally_date','sell_rally_day'),
                 leadvarnames=c('lead1sellrally','lead1sellrallydate')){
  setorder(stock_dat, symbol, date)
  stock_dat[,sell_rally_increment:=shift(sell_rule(.SD),n=1,type='lag'),symbol]
  stock_dat[,sell_rally_increment:=ifelse(is.na(sell_rally_increment),0,sell_rally_increment)]
  stock_dat[,sell_rally_increment:=cumsum(sell_rally_increment), symbol]
  stock_dat[,c(varnames):=list(close[.N],
                               date[.N],
                               seq_len(.N)),.(sell_rally_increment,symbol)]
  stock_dat[,c(leadvarnames):= list(shift(sell_rally,1,type='lead'),
                                shift(sell_rally_date,1,type='lead')),symbol ]
}


rally_avg = function(stock_dat, window){
  sell_rally_avg_column = function(price_dat){
    days=nrow(price_dat)
    return( mean(ifelse(price_dat[,1]>price_dat[days,5], 
                        price_dat[days,2], price_dat[,4])/price_dat[,3]))
  }
  
  stock_dat[symbol %in% stock_dat[,.N,symbol][N>window,symbol],
            sell_rally_avg:= zoo::rollapply(data=.SD[,.(sell_rally_date=as.integer(sell_rally_date),
                                                        close,open,sell_rally,
                                                        date=as.integer(date))],
                                            FUN=sell_rally_avg_column,
                                            width=window, align='right',by.column = FALSE,fill=NA
            ),symbol ]
  }
