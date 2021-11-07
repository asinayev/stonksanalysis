require(data.table)
require(tidyquant)

fundamentals = fread("~/stonksanalysis/other_datasources/nasdaq_screener_1636253557582.csv") #from https://www.nasdaq.com/market-activity/stocks/screener

share_buybacks = jsonlite::fromJSON(txt='')
dividend_creations = jsonlite::fromJSON(txt='')

share_buybacks = data.frame(share_buybacks[c('symbol','event_date_est')])
share_buybacks$eventtype='buyback'
dividend_creations= data.frame(dividend_creations[c('symbol','event_date_est')])
dividend_creations$eventtype='dividient_creation'
eventsdf = rbind(share_buybacks,dividend_creations)

eventsdf$event_date_est=lubridate::ymd_hms(eventsdf$event_date_est, tz = "EST")
eventsdf$is_after_hours = lubridate::hour(eventsdf$event_date_est)>9
eventsdf$during_trading = lubridate::hour(eventsdf$event_date_est)<16 & lubridate::hour(eventsdf$event_date_est)>9
eventsdf$event_date = as.Date(eventsdf$event_date_est, tz = "EST")


system.time(stockdat <- parallel::mclapply(unique(eventsdf$symbol),
                              tq_get,
                              from=min(eventsdf$event_date, na.rm=T),
                              to=max(eventsdf$event_date, na.rm=T)+3,
                              mc.cores = 8))
stockdat = rbindlist(stockdat[lapply(stockdat, is.data.frame)%>%unlist])
setorder(stockdat, symbol, date)

stockdat[,nextopen   :=shift(open, -1),symbol]
stockdat[,nextclose  :=shift(close,-1),symbol]
stockdat[,nexthigh   :=shift(high, -1),symbol]
stockdat[,nextlow    :=shift(low,  -1),symbol]
stockdat[,next2open  :=shift(open, -2),symbol]
stockdat[,next2close :=shift(close,-2),symbol]
stockdat[,next2high  :=shift(high, -2),symbol]
stockdat[,next2low   :=shift(low,  -2),symbol]

events_prices = merge(data.table(eventsdf),
                       stockdat,
                       by.x = c('symbol', "event_date"),
                       by.y = c('symbol','date'), 
                       all.x=T )
events_prices = merge(events_prices,
                      fundamentals[,c('Symbol','Market Cap', 'Country', 'IPO Year', 'Volume', 'Sector', 'Industry')],
                      by.x = c('symbol'),
                      by.y = c('Symbol'), 
                      all.x=T )

events_prices[,gain_day_1:=ifelse( is_after_hours, (nextclose-nextopen)/nextopen, (close-open)/open )]
events_prices[,gain_day_2:=ifelse( is_after_hours, (next2close-next2open)/next2open, (nextclose-nextopen)/nextopen )]
events_prices[,gain_cum:=ifelse( is_after_hours, (next2close-nextopen)/nextopen, (nextclose-open)/open )]

events_prices$gain_cum%>%summary

events_prices[,.(mean(gain_cum,na.rm=T), median(gain_cum, na.rm=T), .N),.(`Market Cap`>25*10^9, eventtype)]
events_prices[,.(mean(gain_cum,na.rm=T), median(gain_cum, na.rm=T), .N),.(Sector,eventtype)][order(N)]
events_prices[,.(mean(gain_cum,na.rm=T), median(gain_cum, na.rm=T), .N),.(is_after_hours,eventtype)][order(N)]
