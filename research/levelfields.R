require(data.table)
require(tidyquant)
require(jsonlite)

args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){args='~/stonksanalysis'}
setwd(args[1])
source("polygon.R", local=T)
POLYKEY = Sys.getenv('POLYGONKEY')

fundamentals = fread("~/stonksanalysis/other_datasources/nasdaq_screener_1636253557582.csv") #from https://www.nasdaq.com/market-activity/stocks/screener

scenarios = c('share_buybacks'=164,
              'dividend_creations'=168,
              'dividend_increase'=121,
              'mass_layoffs'=119,
              'dividend_reduction'=118,
              'short_seller'=161,
              'ceo_departs'=41,
              'class_act'=116,
              'removed_sp'=163)

file_str <- paste(readLines("~/datasets/ceo-departs.mhtml"), collapse="")%>%
  gsub(pattern='=',replacement = '',fixed = T)
stringr::str_extract_all(file_str, 'levelfields\\.ai/e[a-zA-Z/0-9=_]*')

system.time(stockdat <- parallel::mclapply(unique(eventsdf$symbol),
                                           stock_history,
                                           start_date=min(eventsdf$event_date, na.rm=T)-10,
                                           end_date=max(eventsdf$event_date, na.rm=T)+3,
                                           key=POLYKEY,print=F, check_ticker=F,
                                           mc.cores = 8))

stockdat = rbindlist(stockdat[lapply(stockdat, is.data.frame)%>%unlist])
stockdat = stockdat[, .SD[1], by=.(stock, Date)][
  ,.(symbol=stock,date=Date, AdjClose, open, high, low, volume, close=AdjClose)]

setorder(stockdat, symbol, date)

stockdat[,sell_rally_increment:=ifelse(shift(close,n=1,type='lag') <  shift(high,n = 2, type="lag") | 
                                         is.na(shift(high,n = 2, type="lag")), 
                                       0, 1),symbol]
stockdat[,sell_rally_increment:=cumsum(sell_rally_increment), symbol]
stockdat[,sell_rally:=close[.N], .(sell_rally_increment,symbol)]
stockdat[,sell_rally_date:=date[.N], .(sell_rally_increment,symbol)]

stockdat[,nextopen   :=shift(open, -1),symbol]
stockdat[,nextclose  :=shift(close,-1),symbol]
stockdat[,nexthigh   :=shift(high, -1),symbol]
stockdat[,nextlow    :=shift(low,  -1),symbol]
stockdat[,nextsellrally    :=shift(sell_rally,  -1),symbol]
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
events_prices[,gain_rally:=ifelse( is_after_hours, (nextsellrally-nextopen)/nextopen, (sell_rally-open)/open )]
events_prices[,gain_wait:=ifelse(gain_day_1>-.02, gain_day_1, gain_day_5)]
events_prices[,gain_wait_rally:=ifelse(gain_day_1>-.02, gain_day_1, gain_rally)]
events_prices[,gain_wait_fall:=ifelse(gain_day_1> 0, gain_day_1, gain_day_5)]
events_prices[,gain_overnight:= ifelse( is_after_hours, (nextopen-close)/close, (open-prevclose)/prevclose )]
events_prices[,marketcap_num:= as.numeric(marketcap)]
events_prices[,volume_num:= as.numeric(filter_volume)]
events_prices[,prev_vol:= ifelse( is_after_hours, volume*close, prevvolume*prevclose )]
events_prices[sector %in% events_prices[,.N,sector][N<150, sector], sector:='Other']


events_prices[,.(day1 = mean(gain_day_1,na.rm=T),
                 day5 = mean(gain_day_5,na.rm=T),  
                 rally = mean(gain_rally,na.rm=T), 
                 rally_days = mean(sell_rally_date-event_date,na.rm=T), 
                 wait1_5 = mean(gain_wait,na.rm=T),
                 wait_rally = mean(gain_wait_rally,na.rm=T),
                 wait_fall = mean(gain_wait_fall,na.rm=T),
                 wait_fall = median(gain_wait_fall,na.rm=T),
                 day20 = mean(gain_day_20,na.rm=T), .N),
              .( eventtype)]
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
