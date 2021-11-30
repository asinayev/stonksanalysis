require(data.table)
require(tidyquant)
require(rpart)
install.packages('rpart.plot')
install.packages("MASS")

fundamentals = fread("~/stonksanalysis/other_datasources/nasdaq_screener_1636253557582.csv") #from https://www.nasdaq.com/market-activity/stocks/screener

share_buybacks = jsonlite::fromJSON(txt=    'https://app.endpoints.levelfields.ai/scenarios.php?scenario_id=164&key=tmqBmVwcf7Qd5K7LCdBDLG2WV3Uywb&start_date=2010-11-29&end_date=2021-11-29&jwt=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiJ9.eyJpc3MiOiJMRVZFTEZJRUxEUyIsImlhdCI6MTYzODE5NzE2NSwibmJmIjoxNjM4MTk3MTY1LCJleHAiOjE2MzgyMzMxNjUsImRhdGEiOnsiZmlyc3RuYW1lIjoiQWxla3NhbmRyIiwibGFzdG5hbWUiOiJTaW5heWV2IiwiZW1haWwiOiJhc2luYXlldkBnbWFpbC5jb20iLCJyb2xlIjoiVVNFUiIsInJlY3VybHlfdXNlcl9hY2NvdW50X2lkIjoiQWxla3NhbmRyU2luYXlldi02NjdhOGVjZjhkNTRhZWI4NDU4NiIsInJlY3VybHlfcGxhbl9pZCI6ImJldGEwNTkiLCJyZWN1cmx5X3N1YnNjcmlwdGlvbl9pZCI6InB0aDY2OHJ2bGk0cyIsInJlY3VybHlfc3Vic2NyaXB0aW9uX2VuZF9kYXRlIjoiMjAyMS0xMi0wMiAxODo0NToyOSIsImZpc3J0X2xvZ2luIjpmYWxzZSwiaXNfY2FuY2VsbGVkIjpmYWxzZX19.gakTFXtSlQ2d-hh-rhXxlQFxLzsPiCzCFz09_wl3UibZJ5Vzsc0eFLndRPCzQTSM7TqkD7-BD3Wt3SEB2punoA')
dividend_creations = jsonlite::fromJSON(txt='https://app.endpoints.levelfields.ai/scenarios.php?scenario_id=160&key=tmqBmVwcf7Qd5K7LCdBDLG2WV3Uywb&start_date=2010-11-29&end_date=2021-11-29&jwt=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiJ9.eyJpc3MiOiJMRVZFTEZJRUxEUyIsImlhdCI6MTYzODE5NzE2NSwibmJmIjoxNjM4MTk3MTY1LCJleHAiOjE2MzgyMzMxNjUsImRhdGEiOnsiZmlyc3RuYW1lIjoiQWxla3NhbmRyIiwibGFzdG5hbWUiOiJTaW5heWV2IiwiZW1haWwiOiJhc2luYXlldkBnbWFpbC5jb20iLCJyb2xlIjoiVVNFUiIsInJlY3VybHlfdXNlcl9hY2NvdW50X2lkIjoiQWxla3NhbmRyU2luYXlldi02NjdhOGVjZjhkNTRhZWI4NDU4NiIsInJlY3VybHlfcGxhbl9pZCI6ImJldGEwNTkiLCJyZWN1cmx5X3N1YnNjcmlwdGlvbl9pZCI6InB0aDY2OHJ2bGk0cyIsInJlY3VybHlfc3Vic2NyaXB0aW9uX2VuZF9kYXRlIjoiMjAyMS0xMi0wMiAxODo0NToyOSIsImZpc3J0X2xvZ2luIjpmYWxzZSwiaXNfY2FuY2VsbGVkIjpmYWxzZX19.gakTFXtSlQ2d-hh-rhXxlQFxLzsPiCzCFz09_wl3UibZJ5Vzsc0eFLndRPCzQTSM7TqkD7-BD3Wt3SEB2punoA')
dividend_increase= jsonlite::fromJSON(txt=  'https://app.endpoints.levelfields.ai/scenarios.php?scenario_id=121&key=tmqBmVwcf7Qd5K7LCdBDLG2WV3Uywb&start_date=2010-11-29&end_date=2021-11-29&jwt=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiJ9.eyJpc3MiOiJMRVZFTEZJRUxEUyIsImlhdCI6MTYzODE5NzE2NSwibmJmIjoxNjM4MTk3MTY1LCJleHAiOjE2MzgyMzMxNjUsImRhdGEiOnsiZmlyc3RuYW1lIjoiQWxla3NhbmRyIiwibGFzdG5hbWUiOiJTaW5heWV2IiwiZW1haWwiOiJhc2luYXlldkBnbWFpbC5jb20iLCJyb2xlIjoiVVNFUiIsInJlY3VybHlfdXNlcl9hY2NvdW50X2lkIjoiQWxla3NhbmRyU2luYXlldi02NjdhOGVjZjhkNTRhZWI4NDU4NiIsInJlY3VybHlfcGxhbl9pZCI6ImJldGEwNTkiLCJyZWN1cmx5X3N1YnNjcmlwdGlvbl9pZCI6InB0aDY2OHJ2bGk0cyIsInJlY3VybHlfc3Vic2NyaXB0aW9uX2VuZF9kYXRlIjoiMjAyMS0xMi0wMiAxODo0NToyOSIsImZpc3J0X2xvZ2luIjpmYWxzZSwiaXNfY2FuY2VsbGVkIjpmYWxzZX19.gakTFXtSlQ2d-hh-rhXxlQFxLzsPiCzCFz09_wl3UibZJ5Vzsc0eFLndRPCzQTSM7TqkD7-BD3Wt3SEB2punoA')

cols_to_get=c('symbol','event_date_est','event_source','sector','marketcap','filter_volume')
share_buybacks = data.frame(share_buybacks[cols_to_get])
share_buybacks$eventtype='buyback'
dividend_creations= data.frame(dividend_creations[cols_to_get])
dividend_creations$eventtype='dividend_creation'
dividend_increase= data.frame(dividend_increase[cols_to_get])
dividend_increase$eventtype='dividend_increase'
eventsdf = rbind(share_buybacks,dividend_creations,dividend_increase)

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
stockdat[,prevclose  :=shift(close, 1),symbol]
stockdat[,prevvolume :=shift(volume,1),symbol]

events_prices = merge(data.table(eventsdf),
                      stockdat,
                      by.x = c('symbol', "event_date"),
                      by.y = c('symbol','date'),
                      all.x=T )
# events_prices = merge(events_prices,
#                       fundamentals[,c('Symbol','Market Cap', 'Country', 'IPO Year', 'Volume', 'Sector', 'Industry')],
#                       by.x = c('symbol'),
#                       by.y = c('Symbol'),
#                       all.x=T )

events_prices[,gain_day_1:=ifelse( is_after_hours, (nextclose-nextopen)/nextopen, (close-open)/open )]
events_prices[,gain_day_2:=ifelse( is_after_hours, (next2close-next2open)/next2open, (nextclose-nextopen)/nextopen )]
events_prices[,gain_cum:=ifelse( is_after_hours, (next2close-nextopen)/nextopen, (nextclose-open)/open )]
events_prices[,delta_since_prev:= ifelse( is_after_hours, (nextopen-close)/close, (open-prevclose)/prevclose )]
events_prices[,marketcap_num:= as.numeric(marketcap)]
events_prices[,volume_num:= as.numeric(filter_volume)]
events_prices[,prev_vol:= ifelse( is_after_hours, volume*close, prevvolume*prevclose )]
events_prices[sector %in% events_prices[,.N,sector][N<150, sector], sector:='Other']


events_prices$gain_cum%>%summary

events_prices[,.(mean(gain_cum,na.rm=T), median(gain_cum, na.rm=T), .N),.( eventtype)]
events_prices[,.(mean(gain_cum,na.rm=T), median(gain_cum, na.rm=T), .N),.(`Market Cap`>25*10^9, eventtype)]
events_prices[,.(mean(gain_cum,na.rm=T), median(gain_cum, na.rm=T), .N),.(sector,eventtype)][order(N)]
events_prices[,.(mean(gain_cum,na.rm=T), median(gain_cum, na.rm=T), .N),.(event_source,eventtype)][order(N)]
events_prices[,.(mean(gain_cum,na.rm=T), median(gain_cum, na.rm=T), .N),.(is_after_hours,eventtype)][order(N)]

events_prices[eventtype=='dividend_creation',.(mean(gain_cum,na.rm=T),.N),.( round(log(prev_vol)) )][order(round)] %>% with(plot(round,V1,cex=log(N)/2))
abline(h=0)
events_prices[eventtype=='dividend_creation',.(mean(gain_cum,na.rm=T),.N),   round(delta_since_prev,2)][order(round)] %>% with(plot(round,V1,cex=log(N)/2))
abline(h=0)
abline(v=0)
mc=function(x){(x-mean(x,na.rm=T))}

summary(MASS::rlm(gain_day_1~
                    # sector +
                    # event_source + 
                    log(prev_vol+1) +
                    log(marketcap_num+1)+
                    mc(delta_since_prev)
                  , 
                  events_prices[eventtype=='dividend_creation' & event_date_est>"2021-06-01",]))

summary(lm(gain_day_1~
                    (sector=='Financial Services') +
                    log(volume_num+1)
                  , 
                  events_prices[eventtype!='dividend_creation' & event_date_est<"2022-06-01",]))


events_prices[Sector!='Finance' & log(prev_vol) < 17 & eventtype!='dividend_increase' & delta_since_prev > -.01,
              .(mean(gain_cum,na.rm=T), median(gain_cum,na.rm=T),mean(gain_cum>0,na.rm=T), .N )]
events_prices[Sector!='Finance' & log(prev_vol) < 17 & eventtype!='dividend_increase' & delta_since_prev > -.01,
              .(mean(gain_cum,na.rm=T), median(gain_cum,na.rm=T),mean(gain_cum>0,na.rm=T), .N )]

m0 = rpart(gain_day_1 ~ volume_num+event_source+prev_vol+delta_since_prev, 
           events_prices, subset=eventtype!='dividend_creation', 
        control = rpart.control(minbucket =  150, cp=.00000000001))
rpart.plot::rpart.plot(m0)

events_prices[eventtype %in% c('dividient_creation', 'buyback') & `Market Cap`<25*10^9 & Sector!='Finance' & delta_since_prev %between% c(-.01,.1)][order(-event_date_est)]
