require(tidyquant, quietly = T)
require(data.table, quietly = T)

setwd('~/stonksanalysis')
source("polygon.R", local=T)
POLYKEY = Sys.getenv('POLYGONKEY')

get_day = function(date, key, yesterday_news=F){
  
  if(!lubridate::wday(date) %between% c(2,6)){
    return(NULL)
  } else if (lubridate::wday(date)==2){
    yesterday = date-3
  } else {
    yesterday = date-1
  }
  if(yesterday_news){
    open = lubridate::as_datetime(paste(yesterday,"09:30:00",collapse = "T"),tz='America/New_York')
    close = lubridate::as_datetime(paste(yesterday,"15:00:00",collapse = "T"),tz='America/New_York')
  } else {
    open = lubridate::as_datetime(paste(yesterday,"16:00:00",collapse = "T"),tz='America/New_York')
    close = lubridate::as_datetime(paste(date,"09:00:00",collapse = "T"),tz='America/New_York')
  }
  today_news = "https://api.polygon.io/v2/reference/news?published_utc.gt=%s&published_utc.lt=%s&apiKey=%s&limit=1000" %>%
    sprintf(open %>% with_tz('UTC') %>% format("%Y-%m-%dT%H:%M:%S"),
            close %>% with_tz('UTC') %>% format("%Y-%m-%dT%H:%M:%S"), key) %>%
    get_all_results(results_contain = 'published_utc')
  if(!all(c('id', 'publisher', "published_utc", 'title', 'author', 'tickers') %in% names(today_news)) ){
    return(NULL)
  }
  today_news$date=date
  today_news%>%data.table
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

days_to_look_at = as.Date(as.Date("2021-04-20"):Sys.Date(),origin='1970-01-01')

just_news = as.Date(as.Date("2022-04-20"):as.Date("2022-04-29"),origin='1970-01-01') %>%
  lapply(get_day, key=POLYKEY) %>%
  rbindlist(use.names=TRUE, fill=T)

just_news = parallel::mclapply(
  days_to_look_at,
  get_day, key=POLYKEY,mc.cores = 16,
  yesterday_news=F
) %>%
  rbindlist(use.names=TRUE, fill=T)

news_moves = merge(clean_news(just_news),
                   prices, by=c('date','symbol'), all.x=T)

byword = news_moves[!sapply(keywords, is.null),
                    .(keywords=unlist(keywords) ),
                    .(id, delta=c/o, overnight_delta=o/prev_close,
                      prev_delta=prev_close/prev_open, prev_dol_vol = prev_close*prev_vol, open=o,
                      ticker, single_ticker, market_cap, date, publisher.name, title)]

byword[log(market_cap)<21 & keywords %in%c('Health', 'Partnerships', 'Press releases') & publisher.name=='GlobeNewswire Inc.'  & overnight_delta>1.01,
       .(mean(delta,na.rm=T),
         median(delta,na.rm=T),
         length(unique(paste(date,ticker)))),.(year(date),month(date) )][order(year,month,decreasing = T)]
# short penny stocks with GlobeNewswire's "Health" keywords
pennyshort_trades = byword[log(market_cap)<21 & keywords %in%c('Health', 'Partnerships', 'Press releases') & publisher.name=='GlobeNewswire Inc.'  & overnight_delta>1.01,
                           .(date,ticker,open,delta)]
pennyshort_hours = get_hours_for_stocks(pennyshort_trades$ticker,
                                        start_date=min(pennyshort_trades$date),
                                        end_date=Sys.Date(),
                                        key=POLYKEY)
merge(pennyshort_trades, pennyshort_hours,
      by.x=c('ticker','date'),by.y=c('stock','bar_date'))[
        ,.(mean(`9`/open,na.rm=T),
           mean(`10`/open,na.rm=T),
           mean(`11`/open,na.rm=T),
           mean(`14`/open,na.rm=T),
           mean(`15`/open,na.rm=T),
           mean(delta,na.rm=T))]

byword[log(market_cap)<21 & publisher.name=='PennyStocks' & overnight_delta>1.01,
       .(mean(delta,na.rm=T),
         median(delta,na.rm=T),
         length(unique(paste(date,ticker)))),.(year(date), month(date) )][order(month,decreasing = T)]
# short PennyStocks penny stocks with upward movement >1%

pennyshort_trades = byword[log(market_cap)<21 & publisher.name=='PennyStocks' & overnight_delta>1.01,
                           .(date,ticker,open,delta)]
pennyshort_hours = get_hours_for_stocks(pennyshort_trades$ticker,
                                        start_date=min(pennyshort_trades$date),
                                        end_date=Sys.Date(),
                                        key=POLYKEY)
merge(pennyshort_trades, pennyshort_hours,
      by.x=c('ticker','date'),by.y=c('stock','bar_date'))[ !is.na(AdjClose_9)
                                                           ,.(mean(open/Open_9,na.rm=T),
                                                              mean(AdjClose_9/open,na.rm=T),
                                                              mean(AdjClose_10/open,na.rm=T),
                                                              mean(AdjClose_11/open,na.rm=T),
                                                              mean(AdjClose_12/open,na.rm=T),
                                                              mean(AdjClose_13/open,na.rm=T),
                                                              mean(AdjClose_14/open,na.rm=T),
                                                              mean(AdjClose_15/AdjClose_8,na.rm=T),
                                                              mean(Open_16/open,na.rm=T),
                                                              mean(delta,na.rm=T),.N)]


byword[log(market_cap)<21 & publisher.name=='Benzinga' &
         keywords%in%c('Small Cap','Penny Stocks') &
         overnight_delta>1.01,
       .(mean(delta,na.rm=T),
         median(delta,na.rm=T),
         length(unique(paste(date,ticker)))),.(year(date),month(date) )][order(year,month,decreasing = T)]
# short Benzinga's penny stocks with Penny Stocks and Small Cap keywords and upward movement >1%


byword[
  ,
  .(mean(abs(delta-1),na.rm=T),
    mean(delta,na.rm=T),
    median(delta,na.rm=T),
    length(unique(paste(date,ticker)))),.(publisher.name, keywords)][order(V1,decreasing = T)][V4>1000]

unnest_tokens(news_moves[!is.na(single_ticker),.(date,ticker,title, delta=c/o,publisher.name)], bigram, title)[
  bigram=='daily j']

##
## WINNING
##
news_moves[grepl('top|beat|surpass', title, ignore.case = T) &
           grepl('earning', title, ignore.case = T) &
           grepl('revenue', title, ignore.case = T) &
           #& !grepl('financ', name, ignore.case = T)
           publisher.name=='Zacks Investment Research' &
           lag1volume>50000 & lag1close>5 &
           lag1close<100 & 
           lag1close/lag1open <.99 & 
           open/lag1close <= 1
           #& !is.na(single_ticker)
           ,
           max(date),
           .(date,symbol,sell_rally,open)][, .(mean(sell_rally/open),.N), .(year(date))][order(year)]

news_moves[grepl('(earnings).*(revenue)', title, ignore.case = T) &
             o/prev_close>1 & prev_vol>100000 
           & !is.na(single_ticker),max(date),
           .(date,ticker,c,o,market_cap)][, .(mean(c/o),.N),.(year(date),month(date))][order(year,month)]


news_moves[grepl('(new|announce|declare|authori).*(repurchase|buyback)', title, ignore.case = T) & 
             o/prev_close<1.1 & o/prev_close>.98 &prev_vol>75000 & market_cap<10000000000 &
            !is.na(single_ticker),max(date),
           .(date,ticker,c,o,market_cap)][, .(mean(c/o),.N), .(year(date),month(date))][order(year,month)]
