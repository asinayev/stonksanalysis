require(data.table)
require(tidyquant)
require(rpart)
install.packages('rpart.plot')
install.packages("MASS")

fundamentals = fread("~/stonksanalysis/other_datasources/nasdaq_screener_1636253557582.csv") #from https://www.nasdaq.com/market-activity/stocks/screener

https://app.endpoints.levelfields.ai/scenarios.php?scenario_id=164&key=tmqBmVwcf7Qd5K7LCdBDLG2WV3Uywb&start_date=2021-10-27&end_date=2022-01-27&jwt=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiJ9.eyJpc3MiOiJMRVZFTEZJRUxEUyIsImlhdCI6MTY0MzMxOTg0NSwibmJmIjoxNjQzMzE5ODQ1LCJleHAiOjE2NDMzNTU4NDUsImRhdGEiOnsiZmlyc3RuYW1lIjoiQWxla3NhbmRyIiwibGFzdG5hbWUiOiJTaW5heWV2IiwiZW1haWwiOiJhc2luYXlldkBnbWFpbC5jb20iLCJyb2xlIjoiVVNFUiIsInJlY3VybHlfdXNlcl9hY2NvdW50X2lkIjoiQWxla3NhbmRyU2luYXlldi02NjdhOGVjZjhkNTRhZWI4NDU4NiIsInJlY3VybHlfcGxhbl9pZCI6ImJldGEwNTkiLCJyZWN1cmx5X3N1YnNjcmlwdGlvbl9pZCI6InB0aDY2OHJ2bGk0cyIsInJlY3VybHlfc3Vic2NyaXB0aW9uX2VuZF9kYXRlIjoiMjAyMi0wMi0wMiAxODo0NToyOSIsImZpc3J0X2xvZ2luIjpmYWxzZSwiaXNfY2FuY2VsbGVkIjpmYWxzZSwic2hvd19wbGFuX3VwZ3JhZGVfb3B0aW9uIjp0cnVlfX0.z3eerPGWunOnpmDHYvfyg_E3vBE3U253s2lL_tZC4X2edf1IwXYTz4JpnL6p_5W-4fjeS9pkZD4_eUiIJzpI_Q

share_buybacks = jsonlite::fromJSON(txt=    'https://app.endpoints.levelfields.ai/scenarios.php?scenario_id=164&key=tmqBmVwcf7Qd5K7LCdBDLG2WV3Uywb&start_date=2010-11-29&end_date=2022-11-29&jwt=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiJ9.eyJpc3MiOiJMRVZFTEZJRUxEUyIsImlhdCI6MTY0MzMxOTg0NSwibmJmIjoxNjQzMzE5ODQ1LCJleHAiOjE2NDMzNTU4NDUsImRhdGEiOnsiZmlyc3RuYW1lIjoiQWxla3NhbmRyIiwibGFzdG5hbWUiOiJTaW5heWV2IiwiZW1haWwiOiJhc2luYXlldkBnbWFpbC5jb20iLCJyb2xlIjoiVVNFUiIsInJlY3VybHlfdXNlcl9hY2NvdW50X2lkIjoiQWxla3NhbmRyU2luYXlldi02NjdhOGVjZjhkNTRhZWI4NDU4NiIsInJlY3VybHlfcGxhbl9pZCI6ImJldGEwNTkiLCJyZWN1cmx5X3N1YnNjcmlwdGlvbl9pZCI6InB0aDY2OHJ2bGk0cyIsInJlY3VybHlfc3Vic2NyaXB0aW9uX2VuZF9kYXRlIjoiMjAyMi0wMi0wMiAxODo0NToyOSIsImZpc3J0X2xvZ2luIjpmYWxzZSwiaXNfY2FuY2VsbGVkIjpmYWxzZSwic2hvd19wbGFuX3VwZ3JhZGVfb3B0aW9uIjp0cnVlfX0.z3eerPGWunOnpmDHYvfyg_E3vBE3U253s2lL_tZC4X2edf1IwXYTz4JpnL6p_5W-4fjeS9pkZD4_eUiIJzpI_Q')
dividend_creations = jsonlite::fromJSON(txt='https://app.endpoints.levelfields.ai/scenarios.php?scenario_id=168&key=tmqBmVwcf7Qd5K7LCdBDLG2WV3Uywb&start_date=2010-11-29&end_date=2022-11-29&jwt=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiJ9.eyJpc3MiOiJMRVZFTEZJRUxEUyIsImlhdCI6MTY0MzMxOTg0NSwibmJmIjoxNjQzMzE5ODQ1LCJleHAiOjE2NDMzNTU4NDUsImRhdGEiOnsiZmlyc3RuYW1lIjoiQWxla3NhbmRyIiwibGFzdG5hbWUiOiJTaW5heWV2IiwiZW1haWwiOiJhc2luYXlldkBnbWFpbC5jb20iLCJyb2xlIjoiVVNFUiIsInJlY3VybHlfdXNlcl9hY2NvdW50X2lkIjoiQWxla3NhbmRyU2luYXlldi02NjdhOGVjZjhkNTRhZWI4NDU4NiIsInJlY3VybHlfcGxhbl9pZCI6ImJldGEwNTkiLCJyZWN1cmx5X3N1YnNjcmlwdGlvbl9pZCI6InB0aDY2OHJ2bGk0cyIsInJlY3VybHlfc3Vic2NyaXB0aW9uX2VuZF9kYXRlIjoiMjAyMi0wMi0wMiAxODo0NToyOSIsImZpc3J0X2xvZ2luIjpmYWxzZSwiaXNfY2FuY2VsbGVkIjpmYWxzZSwic2hvd19wbGFuX3VwZ3JhZGVfb3B0aW9uIjp0cnVlfX0.z3eerPGWunOnpmDHYvfyg_E3vBE3U253s2lL_tZC4X2edf1IwXYTz4JpnL6p_5W-4fjeS9pkZD4_eUiIJzpI_Q')
dividend_increase= jsonlite::fromJSON(txt=  'https://app.endpoints.levelfields.ai/scenarios.php?scenario_id=121&key=tmqBmVwcf7Qd5K7LCdBDLG2WV3Uywb&start_date=2010-11-29&end_date=2022-11-29&jwt=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiJ9.eyJpc3MiOiJMRVZFTEZJRUxEUyIsImlhdCI6MTY0MzMxOTg0NSwibmJmIjoxNjQzMzE5ODQ1LCJleHAiOjE2NDMzNTU4NDUsImRhdGEiOnsiZmlyc3RuYW1lIjoiQWxla3NhbmRyIiwibGFzdG5hbWUiOiJTaW5heWV2IiwiZW1haWwiOiJhc2luYXlldkBnbWFpbC5jb20iLCJyb2xlIjoiVVNFUiIsInJlY3VybHlfdXNlcl9hY2NvdW50X2lkIjoiQWxla3NhbmRyU2luYXlldi02NjdhOGVjZjhkNTRhZWI4NDU4NiIsInJlY3VybHlfcGxhbl9pZCI6ImJldGEwNTkiLCJyZWN1cmx5X3N1YnNjcmlwdGlvbl9pZCI6InB0aDY2OHJ2bGk0cyIsInJlY3VybHlfc3Vic2NyaXB0aW9uX2VuZF9kYXRlIjoiMjAyMi0wMi0wMiAxODo0NToyOSIsImZpc3J0X2xvZ2luIjpmYWxzZSwiaXNfY2FuY2VsbGVkIjpmYWxzZSwic2hvd19wbGFuX3VwZ3JhZGVfb3B0aW9uIjp0cnVlfX0.z3eerPGWunOnpmDHYvfyg_E3vBE3U253s2lL_tZC4X2edf1IwXYTz4JpnL6p_5W-4fjeS9pkZD4_eUiIJzpI_Q')

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
stockdat[,next5close  :=shift(close,-5),symbol]
stockdat[,next20close  :=shift(close,-20),symbol]
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
events_prices[,gain_day_5:=ifelse( is_after_hours, (next5close-nextopen)/nextopen, (next5close-open)/open )]
events_prices[,gain_day_20:=ifelse( is_after_hours, (next20close-nextopen)/nextopen, (next20close-open)/open )]
events_prices[,gain_overnight:= ifelse( is_after_hours, (nextopen-close)/close, (open-prevclose)/prevclose )]
events_prices[,marketcap_num:= as.numeric(marketcap)]
events_prices[,volume_num:= as.numeric(filter_volume)]
events_prices[,prev_vol:= ifelse( is_after_hours, volume*close, prevvolume*prevclose )]
events_prices[sector %in% events_prices[,.N,sector][N<150, sector], sector:='Other']


events_prices[,.(mean(gain_day_1,na.rm=T),mean(gain_day_5,na.rm=T), mean(gain_day_20,na.rm=T), .N),.( eventtype)]
events_prices[,.(mean(gain_day_1,na.rm=T),mean(gain_day_5,na.rm=T), mean(gain_day_20,na.rm=T), .N),.(eventtype,log(volume_num)>14)]
events_prices[,.(mean(gain_day_1,na.rm=T),mean(gain_day_5,na.rm=T), mean(gain_day_20,na.rm=T), .N),.(sector,eventtype)][order(N)]
events_prices[,.(mean(gain_day_1,na.rm=T),mean(gain_day_5,na.rm=T), mean(gain_day_20,na.rm=T), .N),.(event_source,eventtype)][order(N)]
events_prices[,.(mean(gain_day_1,na.rm=T),mean(gain_day_5,na.rm=T), mean(gain_day_20,na.rm=T), .N),.(is_after_hours,eventtype)][order(N)]

events_prices[eventtype=='buyback',.(mean(gain_day_5,na.rm=T),.N),.( round(log(volume_num)) )][order(round)] %>% with(plot(round,V1,cex=log(N)/2))
abline(h=0)
events_prices[eventtype=='buyback',.(mean(gain_day_1,na.rm=T),.N),   round(gain_overnight,2)][order(round)] %>% with(plot(round,V1,cex=log(N)/2))
abline(h=0)
abline(v=0)
mc=function(x){(x-mean(x,na.rm=T))}

summary(MASS::rlm(gain_day_5~
                    # sector +
                    # event_source + 
                    log(prev_vol+1) +
                    log(marketcap_num+1)+
                    gain_overnight
                  , 
                  events_prices[eventtype=='buyback']))

summary(lm(gain_day_1~
                    (sector=='Financial Services') +
                    mc(log(volume_num+1)) +
                    mc(gain_overnight) +
                    mc(log(marketcap_num+1))
                  , 
                  events_prices[eventtype!='dividend_increase' & event_date_est<"2022-06-01",]))


events_prices[log(volume_num+1) < 14 & eventtype!='dividend_increase' & gain_overnight > -.02,
              .(mean(gain_day_1,na.rm=T), median(gain_day_1,na.rm=T), mean(gain_day_5,na.rm=T), mean(gain_day_20,na.rm=T), .N )]
events_prices[log(volume_num+1) < 14 & eventtype!='dividend_increase' & gain_overnight > -.02,
              .(mean(  gain_day_1, na.rm=T),mean(  gain_day_5, na.rm=T),mean(  gain_day_20, na.rm=T),
                mean(  ifelse(gain_day_1>-.05, gain_day_1, gain_day_5),na.rm=T), 
                mean(  ifelse(gain_day_1>-.03, gain_day_1, gain_day_5),na.rm=T),
                mean(  ifelse(gain_day_1>-.02, gain_day_1, gain_day_5),na.rm=T),
                .N )]

hit_events = events_prices[order(event_date)][ log(volume_num+1) < 14 & eventtype!='dividend_increase' & gain_overnight > -.02 &!is.na(gain_day_1)]
hit_events[,running_mean:=SMA(gain_day_1,n=50)]
plot(hit_events[,.(as.Date(event_date), running_mean)], type='l')


m0 = rpart(gain_day_5 ~ volume_num+event_source+prev_vol+gain_overnight, 
           events_prices, subset=eventtype!='dividend_increase', 
        control = rpart.control(minbucket =  150, cp=.00000000001))
rpart.plot::rpart.plot(m0)

events_prices[eventtype %in% c('dividient_creation', 'buyback') & `Market Cap`<25*10^9 & Sector!='Finance' & gain_overnight %between% c(-.01,.1)][order(-event_date_est)]
