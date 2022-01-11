get_day = function(date, key){
  
  combine_sources = function(day_moves, yesterday_moves, today_news, yesterday_news){

    today_news[,single_ticker:=ifelse(lapply(tickers, length)==1, 
                                    unlist(lapply(tickers, function(x)x[[1]])), 
                                    NA)]
    today_news = today_news[!sapply(tickers, is.null)]
    today_news[sapply(keywords, is.null),keywords:=list("") ]
    if('keywords' %in% names(today_news)){
      today_news = today_news[,.(ticker=unlist(tickers) ), 
                          .(id, publisher.name, published_utc, title, author, single_ticker)] %>%
        merge(today_news[,.(keywords=first(keywords) ), 
                       .(id, publisher.name, published_utc, title, author, single_ticker)], all.x=T)
    } else {
      today_news = today_news[,.(ticker=unlist(tickers), keywords='None' ), 
                          .(id, publisher.name, published_utc, title, author, single_ticker)]
    }
    today_news[,date:=date]
    today_news = today_news[, .SD[1], .(id, ticker)]
    
    merge(today_news, day_moves, by.x='ticker', by.y='T', all.x=T) %>%
      merge(yesterday_moves[,.(`T`,prev_close = c)], 
            by.x='ticker', by.y='T', all.x=T)
  }
  
  if(!lubridate::wday(date) %between% c(2,6)){
    return(NULL)
  } else if (lubridate::wday(date)==2){
    yesterday = date-3
  } else {
    yesterday = date-1
  }
  open = lubridate::as_datetime(paste(yesterday,"09:30:00",collapse = "T"),tz='America/New_York')
  close = lubridate::as_datetime(paste(date,"16:00:00",collapse = "T"),tz='America/New_York')
  
  day_moves = "https://api.polygon.io/v2/aggs/grouped/locale/us/market/stocks/%s?adjusted=true&apiKey=%s" %>%
    sprintf(date, key) %>%
    hit_polygon
  today_news = "https://api.polygon.io/v2/reference/news?published_utc.gt=%s&published_utc.lt=%s&apiKey=%s&limit=1000" %>%
    sprintf(open %>% with_tz('UTC') %>% format("%Y-%m-%dT%H:%M:%S"), 
            close %>% with_tz('UTC') %>% format("%Y-%m-%dT%H:%M:%S"), key) %>%
    hit_polygon(results_contain = 'published_utc')
  yesterday_moves = "https://api.polygon.io/v2/aggs/grouped/locale/us/market/stocks/%s?adjusted=true&apiKey=%s" %>%
    sprintf(yesterday, key) %>%
    hit_polygon
  day_moves = day_moves$results%>%data.table
  today_news = today_news$results%>%data.table
  
  yesterday_moves = yesterday_moves$results%>%data.table
  
  if(! 'T' %in% names(day_moves) || ! 'T' %in% names(yesterday_moves)){return(NULL)}
  
  combine_sources(day_moves, yesterday_moves, today_news)
  }

days_to_look_at = as.Date(as.Date("2021-04-18"):Sys.Date(),origin='1970-01-01')

# news_moves = sample(days_to_look_at, 20) %>%
#   lapply(get_day, key=POLYKEY) %>%
#   rbindlist(use.names=TRUE, fill=T)

news_moves = parallel::mclapply(
  days_to_look_at, 
  get_day, key=POLYKEY,mc.cores = 16) %>% 
  rbindlist(use.names=TRUE, fill=T)
financials = stock_deets_v(POLYKEY, news_moves$ticker, 16)
news_moves = merge(news_moves,data.frame(financials)[,!is.na(names(financials))], 
                   by='ticker')

byword = news_moves[!sapply(keywords, is.null),
                  .(keywords=unlist(keywords) ), 
                  .(id, delta=c/o, overnight_delta=o/prev_close, ticker, single_ticker, market_cap, date, publisher.name)]

byword[log(market_cap)<21 & keywords =='Health' & publisher.name=='GlobeNewswire Inc.',
       .(mean(delta,na.rm=T),
         median(delta,na.rm=T),
         length(unique(paste(date,ticker)))),.(month(date) )][order(month,decreasing = T)]
byword[log(market_cap)<21 & keywords =='Penny Stocks' & publisher.name=='Benzinga',
       .(mean(delta,na.rm=T),
         median(delta,na.rm=T),
         length(unique(paste(date,ticker)))),.(month(date) )][order(month,decreasing = T)]
# short penny stocks with GlobeNewswire's "Health" or Benzinga's "Penny Stocks" keywords (single ticker)

byword[keywords =='investing' & publisher.name=='The Motley Fool' & !is.na(single_ticker) & overnight_delta %between% c(1.02,1.1),
       .(mean(delta,na.rm=T),
         median(delta,na.rm=T),
         length(unique(paste(date,ticker)))),.(month(date), year(date) )][order(month,decreasing = T)]
# Long Motley Fool's investing and Benzinga's movers keywords with OTH increases 2-10% (single ticker)

byword[publisher.name=='PennyStocks' & log(market_cap)<21,
       .(mean(delta,na.rm=T),
         median(delta,na.rm=T),
         length(unique(paste(date,ticker)))),.(publisher.name, month(date))][order(V1,decreasing = T)][V3>200]
# Long PennyStocks' penny stocks that didn't change too much from previous day

news_moves[grepl('Value', title, ignore.case=T),
       .(round(mean(c/o,na.rm=T),3),
         median(c/o,na.rm=T),
         length(unique(paste(date,ticker)))),.(publisher.name)][order(V3,decreasing = T)][1:20]



