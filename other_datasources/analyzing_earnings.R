pull_indicators = function(stock,date, key){
  stock_dat = stock_day(stock,as.Date(date)-1,date,key = key)
  prev_day_volume = sum(stock_dat$Open*stock_dat$volume)
  day_close = stock_dat[DateTime==as.POSIXct(paste(date,"16:00:00"), tz = 'EST'), AdjClose]
  # x[DateTime<as.POSIXct("2021-11-12 09:46:00", tz = 'EST')]
  stock_dat$EMA_5 = EMA(stock_dat$Open, n = 5)
  stock_dat$EMA_30 = EMA(stock_dat$Open, n = 30)
  stock_dat[DateTime==as.POSIXct(paste(date,"09:45:00"), tz = 'EST'),
            .(stock, EMA_5, EMA_30, bought = AdjClose, sold=day_close, prev_day_volume)]
}


get_volume = function(stocklist, end_date=today(), day_range=14){
  system.time(stockdat <- parallel::mclapply(stocklist,
                                             tq_get,
                                             from=end_date-day_range,
                                             to=end_date,
                                             mc.cores = 8))
  stockdat = rbindlist(stockdat[lapply(stockdat, is.data.frame)%>%unlist])
  stockdat[,mean(volume*close),symbol]
}
