install.packages("googlesheets4")
install.packages('plyr')
setwd("stonksanalysis")
source("polygon.R", local=T)

# Sys.setenv(POLYGONKEY= '')
POLYKEY = Sys.getenv('POLYGONKEY')
library(tidyquant)
library(data.table)

pull_indicators = function(stock,date, key){
  stock_dat = stock_day(stock,as.Date(date)-1,date,key = key)
  if(nrow(stock_dat)<2){
    return(stock_dat[1,.(stock, EMA_5=NA, EMA_30=NA, bought = NA, sold=NA, prev_day_volume=NA)])
  }
  prev_day_volume = sum(stock_dat$Open*stock_dat$volume)
  day_close = stock_dat[DateTime<as.POSIXct(paste(date,"16:01:00"), tz = 'EST')][DateTime==max(DateTime), AdjClose]
  # x[DateTime<as.POSIXct("2021-11-12 09:46:00", tz = 'EST')]
  stock_dat$EMA_5 = EMA(stock_dat$Open, n = 5)
  stock_dat$EMA_30 = EMA(stock_dat$Open, n = 30)
  stock_dat[DateTime==as.POSIXct(paste(date,"09:45:00"), tz = 'EST'),
            .(stock, EMA_5, EMA_30, bought = AdjClose, sold=day_close, prev_day_volume)]
}


get_volume = function(stocklist, end_date=today(), day_range=14){
  system.time(stockdat <- parallel::mclapply(unique(stocklist),
                                             tq_get,
                                             from=end_date-day_range,
                                             to=end_date,
                                             mc.cores = 8))
  stockdat = rbindlist(stockdat[lapply(stockdat, is.data.frame)%>%unlist])
  stockdat[,.(volume = mean(volume*close)),symbol]
}


earnings = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Q9KSUuVPVaCFX5DURsna7ZgI0hDFRKO-g0daA5hPKcQ/edit#gid=1003635395", 
                      sheet = 'Earnings')
  
earnings = merge(data.table(earnings), 
                 get_volume(earnings$Symbol), 
                 by.x = 'Symbol',  by.y = 'symbol')
earnings[, trade_date:=as.Date(ifelse(Time=='bmo', as.Date(Date), as.Date(Date+1)))]


polytickers = "https://api.polygon.io/v3/reference/tickers?type=CS&market=stocks&active=true&sort=ticker&order=asc&limit=1000&apiKey=%s" %>%
  sprintf(POLYKEY) %>%
  hit_polygon
allresults = polytickers$results
while(!is.null(polytickers$next_url)){
  newlink = sprintf('%s&apiKey=%s',polytickers$next_url, POLYKEY)
  polytickers = hit_polygon(newlink)
  allresults = rbind(allresults, polytickers$results)
}

avail_earnings = earnings[Symbol %in% allresults$ticker & Time %in% c('amc','bmo')]
avail_earnings = avail_earnings[sample(nrow(avail_earnings))]
sampled_results = plyr::adply(avail_earnings[1:10], 1, function(x){
  Sys.sleep(10)
  pull_indicators(stock = x$Symbol,date = x$trade_date, key = POLYKEY)})
