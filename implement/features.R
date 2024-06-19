lag_lead_roll = function(stock_dat, corr_window, roll_window, short_roll_window, rolling_features=T){
  setorder(stock_dat, symbol, date)
  stock_dat[,c("lag1close", "lag2close", "lead1close", "lead2close", "lead5close"):=shift(close, n = c(1,2,-1,-2,-5), type = "lag"),symbol]
  stock_dat[,c("lag1open",  "lag2open", "lead1open"):=shift(open,  n = c(1:2,-1), type = "lag"),symbol]
  stock_dat[,c("lag1high",  "lag2high", "lead1high" ):=shift(high,  n = c(1,2,-1), type = "lag"),symbol]
  stock_dat[,c("lag1low",   "lag2low"  ):=shift(low,   n = 1:2, type = "lag"),symbol]
  stock_dat[,c("lag1volume"  ):=shift(volume,   n = 1, type = "lag"),symbol]
  
  if(rolling_features){
    stock_dat[,unbroken_session:=cumsum(10<date-shift(date,n=1,type='lag', fill=as.Date('1970-01-01'))),symbol]
    stock_dat[,days_around:=cumsum(!is.na(close)),.(symbol,unbroken_session)]
    
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
    stock_dat[symbol %in% stock_dat[,.N,symbol][N>roll_window,symbol],
           RSI_short:=RSI(close,n=short_roll_window),symbol ]
    stock_dat[(symbol %in% stock_dat[!is.na(close),.N,symbol][N>26,symbol]) & !is.na(close),
           MACD:=EMA(close ,n = 12, align='right',fill=NA)/
             EMA(close ,n = 26, align='right',fill=NA),symbol ]
    stock_dat[(symbol %in% stock_dat[!is.na(MACD),.N,symbol][N>10,symbol]) & !is.na(MACD),
           MACD_slow:=EMA(MACD ,n = 9, align='right',fill=NA),symbol ]
    stock_dat[,day_drop_norm:=(high-close)/avg_range]
    stock_dat[,day_rise_norm:=(close-low)/avg_range]
    stock_dat[symbol %in% stock_dat[,.N,symbol][N>corr_window,symbol],
           max_volume:= zoo::rollapply(volume,max,width=corr_window, align='right',fill=NA),symbol ]
    stock_dat[symbol %in% stock_dat[,.N,symbol][N>roll_window,symbol],
           avg_vp:= frollmean(close*volume ,n = roll_window, align='right',fill=NA),symbol ]
    stock_dat[order(avg_vp,    decreasing=T),vp_order :=seq_len(.N),date]
    stock_dat[order(market_cap,decreasing=T),cap_order:=seq_len(.N),date]
  }
  setorder(stock_dat, symbol, date)
}

regression_features = function(stock_dat){
  stock_dat[,day_delta:= close/open]
  stock_dat[,day_fall:= low/open]
  stock_dat[,day_rise:= high/open]
  stock_dat[,night_delta:= open/lag1close]
  stock_dat[,future_day_delta:= lead1close/lead1open]
}

rally = function(stock_dat,
                 sell_rule=function(dat){dat$close>=dat$lag1high},
                 varname='sell_rally',
                 sell_close=T){
  setorder(stock_dat, symbol, date)
  stock_dat[,sell_rally_increment:=shift(sell_rule(.SD),n=1,type='lag'),symbol]
  stock_dat[,sell_rally_increment:=ifelse(is.na(sell_rally_increment),0,sell_rally_increment)]
  stock_dat[,sell_rally_increment:=cumsum(sell_rally_increment), symbol]
  varnames = paste0(varname,c('','_date','_day'))
  if(sell_close==T){
    stock_dat[,c(varnames):=list(close[.N],date[.N],seq_len(.N)),
              .(sell_rally_increment,symbol)]
  } else {
    stock_dat[,c(varnames):=list(open[.N],date[.N],seq_len(.N)),
              .(sell_rally_increment,symbol)]
  }
  stock_dat[,sell_rally_increment:=NULL]
}


rally_avg = function(stock_dat, window){
  #The true sell_rally value is not known
  #before the sell_rally_date. If we use the
  #true value to compute the rolling average,
  #this will leak some data into our calculation.
  setorder(stock_dat, symbol, date)
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

key_etfs = function(stock_dat, 
                    key_etfs=c('safe large cap'='JEPI', 'large govnt bonds'='AGG', 'oil'='USO', 
                               'gold'='OUNZ', 'china'='FXI', 'tech'='WCLD', 
                               'near term bonds'='SPSB', 'small cap value'='VBR'),
                    low_corr_thresh=.5){
  stock_dat[,fullday_delta:=close/lag1close]
  prices_wide = stock_dat %>%
    dcast(date~symbol, value.var='fullday_delta',fun.aggregate = mean) 
  price_corrs = data.frame(prices_wide)[,names(prices_wide) != 'date']%>% 
    cor(use='pairwise.complete')
  rows_w_values = !apply(price_corrs,1,function(x)all(is.na(x)))
  price_corrs=abs(price_corrs[rows_w_values,rows_w_values])
  low_corr = apply(price_corrs[,key_etfs] ,1,max,na.rm=T)<low_corr_thresh
  etf_mapper=data.table(key_etf = key_etfs[unlist(apply(price_corrs[,key_etfs],1,which.max))],
                        symbol = colnames(price_corrs))
  etf_mapper[low_corr,key_etf:='none']
  print("deleting %s ETFs due to no correlation" %>%
          sprintf(sum(!rows_w_values)))
  out=merge(stock_dat, etf_mapper)
  setorder(out, symbol, date)
  return(out)
}

clean_news = function(news){
  news[,single_ticker:=ifelse(lapply(tickers, length)==1,
                              unlist(lapply(tickers, function(x)x[[1]])),
                              NA)]
  news[sapply(keywords, is.null),keywords:=list("") ]
  news[sapply(tickers, is.logical),tickers:=list("") ]
  news = news[,.(symbol=unlist(tickers) ),
              .(id, publisher.name, date, title, author, single_ticker)]%>%
    merge(news[,.(keywords=first(keywords) ),
               .(id, publisher.name, date, title, author, single_ticker)], all.x=T)
  news
}

only_passing = function(stock_dat, min_close, min_volume, last_n, passing_date=max(stock_dat$date,na.rm=T)){
  passing_symbols=stock_dat[close>min_close & volume>min_volume & date==passing_date, symbol]
  passing_dat = stock_dat[symbol %in% passing_symbols]
  if(last_n){
    passing_dat = passing_dat[,head(.SD[order(date,decreasing=T)],last_n) ,symbol]
    }
  setorder(passing_dat, symbol, date)
  passing_dat
}