lag_lead_roll = function(stock_dat, corr_window, roll_window, short_roll_window, rolling_features=T){
  setorder(stock_dat, symbol, date)
  
  stock_dat[,unbroken_session:=cumsum(10<date-shift(date,n=1,type='lag', fill=as.Date('1970-01-01'))),symbol]
  stock_dat[,days_around:=cumsum(!is.na(close)),.(symbol,unbroken_session)]
  stock_dat[,symbol_session:=paste0(symbol,'_',unbroken_session)]
  
  stock_dat[,c("lag1close", "lag2close", "lag5close", "lead1close", "lead2close", "lead5close"):=shift(close, n = c(1,2,5,-1,-2,-5), type = "lag"),symbol_session]
  stock_dat[,c("lag1open",  "lag2open", "lead1open"):=shift(open,  n = c(1:2,-1), type = "lag"),symbol_session]
  stock_dat[,c("lag1high",  "lag2high", "lead1high" ):=shift(high,  n = c(1,2,-1), type = "lag"),symbol_session]
  stock_dat[,c("lag1low",   "lag2low"  ):=shift(low,   n = 1:2, type = "lag"),symbol_session]
  stock_dat[,c("lag1volume"  ):=shift(volume,   n = 1, type = "lag"),symbol_session]
  
  if(rolling_features){
    stock_dat[,is_valid:=symbol_session %in% stock_dat[,.N,symbol_session][N>roll_window,symbol_session]]
    stock_dat[is_valid==T
              ,avg_delta:= SMA(close/lag1close, n = roll_window ),symbol_session ]
    stock_dat[is_valid==T
              ,avg_delta_short:= SMA(close/lag1close, n = short_roll_window ),symbol_session ]
    stock_dat[is_valid==T
              ,running_low:= zoo::rollapply(low,min,width=roll_window, align='right',fill=NA),symbol_session ]
    stock_dat[is_valid==T
              ,avg_range:= frollmean(high-low ,n = roll_window, align='right',fill=NA),symbol_session ]
    stock_dat[is_valid==T
              ,sd_from0:= frollmean((1-close/lag1close)^2 ,n = roll_window, align='right',fill=NA)^.5,symbol_session ]
    stock_dat[is_valid==T
              ,avg_volume:= frollmean(volume ,n = roll_window, align='right',fill=NA),symbol ]
    stock_dat[symbol_session %in% stock_dat[,.N,symbol_session][N>(corr_window+short_roll_window), unique(symbol_session)],
              lagging_corr_long:=
                runCor( close/open, close/lag1close, corr_window),
              symbol_session]
    stock_dat[(symbol_session %in% stock_dat[!is.na(close),.N,symbol_session][N>26,symbol_session]) & !is.na(close),
           MACD:=EMA(close ,n = 12, align='right',fill=NA)/
             EMA(close ,n = 26, align='right',fill=NA),symbol_session ]
    stock_dat[(symbol_session %in% stock_dat[!is.na(MACD),.N,symbol_session][N>10,symbol_session]) & !is.na(MACD),
           MACD_slow:=EMA(MACD ,n = 9, align='right',fill=NA),symbol_session ]
    stock_dat[,day_drop_norm:=(high-close)/avg_range]
    stock_dat[symbol_session %in% stock_dat[,.N,symbol_session][N>corr_window,symbol_session],
           max_volume:= zoo::rollapply(volume,max,width=corr_window, align='right',fill=NA),symbol_session ]
    stock_dat[symbol_session %in% stock_dat[,.N,symbol_session][N>corr_window,symbol_session],
          max_price_short:= zoo::rollapply(high,max,width=short_roll_window, align='right',fill=NA),symbol_session ]
    
    stock_dat[is_valid==T
              ,avg_vp:= frollmean(close*volume ,n = roll_window, align='right',fill=NA),symbol_session ]
    stock_dat[order(avg_vp,    decreasing=T),vp_order :=seq_len(.N),date]
    stock_dat[order(market_cap,decreasing=T),cap_order:=seq_len(.N),date]
    
    cube_root_workaround = function(x){
      ifelse(x>=0, x^(1/3),-((-x)^(1/3)))
    }
    
    stock_dat[is_valid==T
           ,avg_root_delta:= cube_root_workaround(SMA((close/lag1close-1)^3, n = short_roll_window ))
           ,symbol_session ]
    stock_dat[is_valid==T
           ,avg_root_delta_lag:= shift(avg_root_delta, short_roll_window)
           ,symbol_session ]
    stock_dat[symbol_session %in% stock_dat[,.N,symbol_session][N>(corr_window+short_roll_window*2), unique(symbol_session)],
           root_delta_corr:=
             runCor( close/lag5close, avg_root_delta_lag, corr_window),
           symbol_session]
  }
  setorder(stock_dat, symbol, date)
}

rally = function(stock_dat,
                 sell_rule=function(dat){dat$close>=dat$lag1high},
                 varname='sell_rally',
                 sell_close=T){
  setorder(stock_dat, symbol, date)
  stock_dat[,sell_rally_increment:=shift(sell_rule(.SD),n=1,type='lag'),symbol_session]
  stock_dat[,sell_rally_increment:=ifelse(is.na(sell_rally_increment),0,sell_rally_increment)]
  stock_dat[,sell_rally_increment:=cumsum(sell_rally_increment), symbol_session]
  varnames = paste0(varname,c('','_date','_day'))
  if(sell_close==T){
    stock_dat[,c(varnames):=list(close[.N],date[.N],seq_len(.N)),
              .(sell_rally_increment,symbol_session)]
  } else {
    stock_dat[,c(varnames):=list(open[.N],date[.N],seq_len(.N)),
              .(sell_rally_increment,symbol_session)]
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
  
  stock_dat[symbol_session %in% stock_dat[,.N,symbol_session][N>window,symbol_session],
            sell_rally_avg:= zoo::rollapply(data=.SD[,.(sell_rally_date=as.integer(sell_rally_date),
                                                        close,open,sell_rally,
                                                        date=as.integer(date))],
                                            FUN=sell_rally_avg_column,
                                            width=window, align='right',by.column = FALSE,fill=NA
            ),symbol_session ]
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

reference_etfs=c('ACWI','AGG','DIA','EEM',
                 'EFA','EWC','EWJ','EWY','EWZ',
                 'FXI','HYG','GDX','GDXJ',
                 'IAU','IBB','IEI','IJR','ITA','IWM','IWP','IWO','IWR','IYF','IYR','IYW',
'KRE','LQD','MBB','QQQ',
                 'SHY','SJNK','SLQD','SLV','SLVP','SOXX','SPHB','SPY','SPYG','STPZ',
'TIP','TLT','UNG','USIG','USO',
                 'VBR','VDC','VEA','VGK','VIXY','VNQ','VTV','VXUS',
'XBI','XLB','XLE','XLF','XLI','XLK','XLU','XLV','XLY','XOP')

check_corr = function(dataset, target_etf, reference_etf){
  m1=dataset[symbol==target_etf,.(date,target_delta_short=avg_delta_short-1)] %>%
    merge(dataset[symbol==reference_etf,.(date,reference_delta_short=avg_delta_short-1)])%>%
    lm(formula=target_delta_short~0+reference_delta_short)
  c('mult'=m1$coefficients, 'rsq'=summary(m1)$r.squared)
}

matching_pairs_for_year = function(yr, dataset=prices, reference_etfs=reference_etfs){
  dataset[,target_var:=avg_delta_short-1]
  key_etfs = dataset[year(date)==(yr-1),
                     .(volume = mean(volume,na.rm=T),
                       volatility = sd(target_var,na.rm=T),
                       count = .N),
                     symbol][volume>100000 & count>50]
  key_etf_wide = dataset[symbol %in% key_etfs$symbol & !is.na(target_var) & year(date)==(yr-1) ] %>%
    dcast(date~symbol, value.var='target_var',fun.aggregate = mean)
  
  etf_corrs = data.frame(key_etf_wide)[,names(key_etf_wide)!='date']%>%
    cor(use='pairwise.complete') %>%
    data.table
  hi_corr=which((etf_corrs>.98 | etf_corrs< -.98),arr.ind=T)
  hi_corr=data.table(
    target = names(etf_corrs)[hi_corr[,1]],
    reference = names(etf_corrs)[hi_corr[,2]]
  ) %>%
    merge(key_etfs[,.(symbol,target_volume=volume, target_volatility=volatility )],
          by.x='target',by.y='symbol')%>%
    merge(key_etfs[,.(symbol,reference_volume=volume, reference_volatility=volatility )],
          by.x='reference',by.y='symbol')
  
  hi_corr_tradeable = hi_corr[(reference_volume>target_volume) & (reference %in% reference_etfs),
                              .(reference,target)]
  hi_corr_tradeable = cbind(hi_corr_tradeable,
                            t(apply(hi_corr_tradeable,1,
                                    function(dats) check_corr(dataset[year(date)==(yr-1)],dats['target'],dats['reference']))))
  comparison_to_reference=dataset[year(date)==yr] %>%
    merge(hi_corr_tradeable, by.x='symbol',by.y = 'target')%>%
    merge(dataset[year(date)==yr,
                  .(date,reference=symbol,reference_delta_short=avg_delta_short,reference_avg_delta=avg_delta, reference_delta=close/lag1close)],
          by=c('date','reference') )
  comparison_to_reference
}
