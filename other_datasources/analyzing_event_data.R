require(data.table)
require(tidyquant)

key = ''
share_buybacks = jsonlite::fromJSON(txt='https://app.endpoints.levelfields.ai/scenarios.php?scenario_id=164&key=  &start_date=2015-11-05&end_date=2021-11-05&jwt=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiJ9.eyJpc3MiOiJMRVZFTEZJRUxEUyIsImlhdCI6MTYzNjEzMjgxNCwibmJmIjoxNjM2MTMyODE0LCJleHAiOjE2MzYxNjg4MTQsImRhdGEiOnsiZmlyc3RuYW1lIjoiQWxla3NhbmRyIiwibGFzdG5hbWUiOiJTaW5heWV2IiwiZW1haWwiOiJhc2luYXlldkBnbWFpbC5jb20iLCJyb2xlIjoiVVNFUiIsInJlY3VybHlfdXNlcl9hY2NvdW50X2lkIjoiQWxla3NhbmRyU2luYXlldi02NjdhOGVjZjhkNTRhZWI4NDU4NiIsInJlY3VybHlfcGxhbl9pZCI6ImJldGEwNTkiLCJyZWN1cmx5X3N1YnNjcmlwdGlvbl9pZCI6InB0aDY2OHJ2bGk0cyIsInJlY3VybHlfc3Vic2NyaXB0aW9uX2VuZF9kYXRlIjoiMjAyMS0xMi0wMiAxODo0NToyOSIsImZpc3J0X2xvZ2luIjpmYWxzZSwiaXNfY2FuY2VsbGVkIjpmYWxzZX19.P7PV3lRCiVhpb49AL5koU32v1mU2FFj4-7bGbMt4chKv_l8eMhRYW9gTZIJMFNXh1HdITAXQXTBjQ-2egDRwgA')
dividend_creations = jsonlite::fromJSON(txt='https://app.endpoints.levelfields.ai/scenarios.php?scenario_id=168&key=  &start_date=2015-11-05&end_date=2021-11-05&jwt=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiJ9.eyJpc3MiOiJMRVZFTEZJRUxEUyIsImlhdCI6MTYzNjEzMjgxNCwibmJmIjoxNjM2MTMyODE0LCJleHAiOjE2MzYxNjg4MTQsImRhdGEiOnsiZmlyc3RuYW1lIjoiQWxla3NhbmRyIiwibGFzdG5hbWUiOiJTaW5heWV2IiwiZW1haWwiOiJhc2luYXlldkBnbWFpbC5jb20iLCJyb2xlIjoiVVNFUiIsInJlY3VybHlfdXNlcl9hY2NvdW50X2lkIjoiQWxla3NhbmRyU2luYXlldi02NjdhOGVjZjhkNTRhZWI4NDU4NiIsInJlY3VybHlfcGxhbl9pZCI6ImJldGEwNTkiLCJyZWN1cmx5X3N1YnNjcmlwdGlvbl9pZCI6InB0aDY2OHJ2bGk0cyIsInJlY3VybHlfc3Vic2NyaXB0aW9uX2VuZF9kYXRlIjoiMjAyMS0xMi0wMiAxODo0NToyOSIsImZpc3J0X2xvZ2luIjpmYWxzZSwiaXNfY2FuY2VsbGVkIjpmYWxzZX19.P7PV3lRCiVhpb49AL5koU32v1mU2FFj4-7bGbMt4chKv_l8eMhRYW9gTZIJMFNXh1HdITAXQXTBjQ-2egDRwgA')

share_buybacks = data.frame(share_buybacks[c('symbol','event_date_est')])
share_buybacks$eventtype='buyback'
dividend_creations= data.frame(dividend_creations[c('symbol','event_date_est')])
dividend_creations$eventtype='dividient_creation'
eventsdf = rbind(share_buybacks,dividend_creations)

eventsdf$event_date_est=lubridate::ymd_hms(eventsdf$event_date_est, tz = "EST")
eventsdf$is_after_hours = lubridate::hour(eventsdf$event_date_est)>15
eventsdf$during_trading = lubridate::hour(eventsdf$event_date_est)<16 & lubridate::hour(eventsdf$event_date_est)>9
eventsdf$event_date = as.Date(eventsdf$event_date_est)


system.time(stockdat <- parallel::mclapply(unique(eventsdf$symbol),
                              tq_get,
                              from=min(eventsdf$next_trade_date, na.rm=T),
                              to=max(eventsdf$next_trade_date, na.rm=T),
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

events_prices[,gain_day_1:=ifelse( is_after_hours, (nextopen-nextclose)/nextopen )]
events_prices[,gain_day_2:=ifelse( is_after_hours, (next2open-next2close)/next2open )]
shares_buyback[,gain_cum:=(close.y-open.x)/open.x]
shares_buyback[,low_cum:=pmin(low.x,low.y)]
shares_buyback[,high_cum:=pmax(high.x,high.y)]

shares_buyback$gain_cum%>%summary
shares_buyback[,gain_cum:=ifelse( (high_cum - open.x)/open.x > .1, 
                                  .1,
                                  gain_cum)]
shares_buyback$gain_cum%>%summary
