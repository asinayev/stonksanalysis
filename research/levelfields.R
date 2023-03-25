require(data.table)
require(tidyquant)
require(jsonlite)

args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){args='~/stonksanalysis'}
setwd(args[1])
source("polygon.R", local=T)
source("implement/features.R", local=T)
source("research/performance.R", local=T)
POLYKEY = Sys.getenv('POLYGONKEY')

scenarios = c('stock-buybacks'=164,
              #'dividend_creations'=168,
              #'dividend_increase'=121,
              'mass_layoffs'=119,
              'dividend-reduction'=118,
              #'short-seller'=161,
              'ceo-departs'=41,
              'class-action-lawsuit'=116#,
              #'removed_sp'=163
              )
pull_scenario = function(scenario){
  links = paste0("~/datasets/",scenario,".mhtml",collapse='') %>%
    readLines()%>%
    paste(collapse='')%>%
    gsub(pattern='=',replacement = '',fixed = T)%>%
    stringr::str_extract_all('levelfields\\.ai/e[a-zA-Z/0-9=_]*')%>%
    unlist
  print(mean(sapply(strsplit(links,'/'), length)==7))
  tabled = links[sapply(strsplit(links,'/'), length)==7]%>%
    strsplit('/')%>%
    do.call(what=data.table)%>%
    t
  data.table(symbol=tabled[,3],
             source=tabled[,4],
             time=tabled[,7],
             eventtype=scenario,
             event_time=as.POSIXct(as.numeric(tabled[,7])/1000, 
                                   origin="1970-01-01", tz = 'EST'),
             event_date=as_date(as.POSIXct(as.numeric(tabled[,7])/1000, 
                                           origin="1970-01-01", tz = 'EST'))
             )

}
eventsdf = scenarios %>% names %>% lapply(pull_scenario) %>% rbindlist(fill=T) 

system.time(stockdat <- parallel::mclapply(unique(eventsdf$symbol),
                                           stock_history,
                                           start_date=min(eventsdf$event_date, na.rm=T)-10,
                                           end_date=Sys.Date(),
                                           key=POLYKEY,print=F, check_ticker=F,
                                           mc.cores = 8))

stockdat = rbindlist(stockdat[lapply(stockdat, is.data.frame)%>%unlist])
stockdat = stockdat[, .SD[1], by=.(stock, Date)][
  ,.(symbol=stock,date=Date, AdjClose, open, high, low, volume, close=AdjClose)]

setorder(stockdat, symbol, date)

lag_lead_roll(stockdat, corr_window=100, roll_window=25, short_roll_window=5)
rally(stockdat)
stockdat[,lead1sell_rally:= shift(sell_rally,1,type='lead'),symbol]
stockdat[,lead1sell_rallydate:= shift(sell_rally_date,1,type='lead'),symbol]
stockdat[,c("lead20close"  ):=shift(close,   n = 20, type = "lead"),symbol]

stocklist = stocklist_from_polygon(key = POLYKEY, date = Sys.Date()-1, details=T, financials=F, cores=16)

events_prices = merge(data.table(eventsdf),
                      stockdat,
                      by.x = c('symbol', "event_date"),
                      by.y = c('symbol','date'),
                      all.x=T ) %>% 
  merge(stocklist[,.(symbol=ticker, market_cap)],
        by='symbol',all.x=T)

events_prices[,is_after_hours:=hour(event_time)>15]
events_prices[,during_trading:=hour(event_time)%between%c(9,15)]
events_prices[,gain_day_1:=ifelse( is_after_hours, (lead1close-lead1open)/lead1open, (close-open)/open )]
events_prices[,gain_day_5:=ifelse( is_after_hours, (lead5close-lead1open)/lead1open, (lead5close-open)/open )]
events_prices[,gain_day_20:=ifelse( is_after_hours, (lead20close-lead1open)/lead1open, (lead20close-open)/open )]
events_prices[,gain_rally:=ifelse( is_after_hours, (lead1sell_rally-lead1open)/lead1open, (sell_rally-open)/open )]
events_prices[,gain_lowclose:=ifelse( is_after_hours, (lead1sell_lowclose-lead1open)/lead1open, (sell_lowclose-open)/open )]
events_prices[,gain_wait:=ifelse(gain_day_1>-.02, gain_day_1, gain_day_5)]
events_prices[,gain_wait_rally:=ifelse(gain_day_1>-.02, gain_day_1, gain_rally)]
events_prices[,gain_wait_fall:=ifelse(gain_day_1> 0, gain_day_1, gain_day_5)]
events_prices[,gain_overnight:= ifelse( is_after_hours, (lead1open-close)/close, (open-lag1close)/lag1close )]
events_prices[,prev_vol:= ifelse( is_after_hours, volume*close, lag1volume*lag1close )]


events_prices[lag1close>6&lag1volume>50000,.(day1 = round(mean(gain_day_1,na.rm=T),3),
                 day5 = round(mean(gain_day_5,na.rm=T),3),  
                 rally = round(mean(gain_rally,na.rm=T),3), 
                 lowclose = round(mean(gain_lowclose,na.rm=T),3), 
                 #rally_days = mean(sell_rally_date-event_date,na.rm=T), 
                 #wait1_5 = mean(gain_wait,na.rm=T),
                 #wait_rally = mean(gain_wait_rally,na.rm=T),
                 #wait_fall = mean(gain_wait_fall,na.rm=T),
                 #wait_fall = median(gain_wait_fall,na.rm=T),
                 day20 = round(mean(gain_day_20,na.rm=T),3), 
                 .N,  
                 rally = round(mean(gain_rally,na.rm=T),3), 
                 lowclose = round(mean(gain_lowclose,na.rm=T),3)),
              .( eventtype, market_cap<10000000000, gain_overnight> 0)][order(eventtype,market_cap,gain_overnight)]
events_prices[,.(mean(gain_day_1,na.rm=T),mean(gain_day_5,na.rm=T), 
                 mean(gain_rally,na.rm=T), mean(gain_day_20,na.rm=T), .N),
              .(eventtype,log(volume_num)>14)][order(eventtype,log)]
events_prices[,.(mean(gain_day_1,na.rm=T),mean(gain_day_5,na.rm=T), 
                 mean(gain_rally,na.rm=T), mean(gain_day_20,na.rm=T), .N),
              .(sector,eventtype)][order(N)]
events_prices[,.(mean(gain_day_1,na.rm=T),mean(gain_day_5,na.rm=T), 
                 mean(gain_rally,na.rm=T), mean(gain_day_20,na.rm=T), .N),
              .(event_source,eventtype)][order(N)]
events_prices[,.(mean(gain_day_1,na.rm=T),mean(gain_day_5,na.rm=T), 
                 mean(gain_rally,na.rm=T), mean(gain_day_20,na.rm=T), .N),
              .(is_after_hours,eventtype)][order(N)]
bad_events=c('short_seller','dividend_reduction','class_act','ceo_departs','mass_layoffs')
events_prices[eventtype%in%bad_events & gain_overnight<0,
              .(mean(gain_rally,na.rm=T),.N),
              .( round(log(marketcap_num)) )][order(round)] %>% 
  with(plot(round,V1,cex=log(N)/2))
abline(h=0)
events_prices[eventtype%in%bad_events,.(mean(gain_day_1,na.rm=T),.N),   
              round(gain_overnight,2)][order(round)] %>% 
  with(plot(round,V1,cex=log(N)/2,xlim=c(-.1,.1),ylim=c(-.1,.1)))
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
                  events_prices[eventtype=='share_buybacks']))

summary(lm(gain_rally~
                    #(sector=='Financial Services') +
                    #mc(log(volume_num+1)) +
                    #mc(gain_overnight) +
                    mc(log(marketcap_num+1))
                  , 
                  events_prices[eventtype=='share_buybacks' & gain_overnight>-.02]))

summary(lm(gain_day_5~
             #(sector=='Financial Services') +
             mc(log(volume_num+1)) 
             #mc(gain_overnight) +
             #mc(log(marketcap_num+1))
           , 
           events_prices[eventtype%in%bad_events & gain_overnight<0]))


events_prices[log(volume_num+1) < 14 & eventtype!='dividend_increase' & gain_overnight > -.02,
              .(mean(gain_day_1,na.rm=T), median(gain_day_1,na.rm=T), mean(gain_day_5,na.rm=T), mean(gain_day_20,na.rm=T), .N )]
events_prices[eventtype!='dividend_increase' & gain_overnight > -.02,
              .(mean(  gain_day_1, na.rm=T),mean(  gain_day_5, na.rm=T),mean(  gain_day_20, na.rm=T),
                mean(  ifelse(gain_day_1>-.05, gain_day_1, gain_day_5),na.rm=T), 
                mean(  ifelse(gain_day_1>-.03, gain_day_1, gain_day_5),na.rm=T),
                mean(  ifelse(gain_day_1>-.02, gain_day_1, gain_day_5),na.rm=T),
                mean(  ifelse(gain_day_1>0, gain_day_1, gain_day_5),na.rm=T),
                mean(  gain_rally),
                .N )]

hit_events = events_prices[order(event_date)][ log(volume_num+1) < 14 & eventtype!='dividend_increase' & gain_overnight > -.02 &!is.na(gain_rally)&!during_trading]
hit_events[,running_mean:=SMA(gain_rally,n=100)]
plot(hit_events[,.(as.Date(event_date), running_mean)], type='l')


m0 = rpart(gain_day_5 ~ volume_num+event_source+prev_vol+gain_overnight, 
           events_prices, subset=eventtype!='dividend_increase', 
        control = rpart.control(minbucket =  150, cp=.00000000001))
rpart.plot::rpart.plot(m0)

events_prices[eventtype %in% c('dividient_creation', 'buyback') & `Market Cap`<25*10^9 & Sector!='Finance' & gain_overnight %between% c(-.01,.1)][order(-event_date_est)]
