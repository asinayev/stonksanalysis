get_day = function(date, key){
  
  combine_sources = function(day_moves, yesterday_moves, today_news, yesterday_news){
    yesterday_news[published_utc %>% as_datetime() %>% with_tz('America/New_York') %>% hour() >15]
    today_news[published_utc %>% as_datetime() %>% with_tz('America/New_York') %>% hour() <9]
    day_news = rbind(yesterday_news, today_news, fill=T)
    
    day_news[,single_ticker:=ifelse(lapply(tickers, length)==1, 
                                    unlist(lapply(tickers, function(x)x[[1]])), 
                                    NA)]
    if('keywords' %in% names(day_news)){
      day_news = day_news[!sapply(tickers, is.null),
                 .(ticker=unlist(tickers), keywords=first(keywords) ), 
                 .(id, publisher.name, published_utc, title, author, single_ticker)]
    } else {
      day_news = day_news[!sapply(tickers, is.null),
                          .(ticker=unlist(tickers), keywords='None' ), 
                          .(id, publisher.name, published_utc, title, author, single_ticker)]
    }
    day_news[,date:=date]
    day_news = day_news[, .SD[1], .(id, ticker)]
    
    merge(day_news, day_moves, by.x='ticker', by.y='T', all.x=T) %>%
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
  day_moves = "https://api.polygon.io/v2/aggs/grouped/locale/us/market/stocks/%s?adjusted=true&apiKey=%s" %>%
    sprintf(date, key) %>%
    hit_polygon
  today_news = "https://api.polygon.io/v2/reference/news?published_utc=%s&apiKey=%s&limit=1000" %>%
    sprintf(date, key) %>%
    hit_polygon(results_contain = 'published_utc')
  yesterday_moves = "https://api.polygon.io/v2/aggs/grouped/locale/us/market/stocks/%s?adjusted=true&apiKey=%s" %>%
    sprintf(yesterday, key) %>%
    hit_polygon
  yesterday_news = "https://api.polygon.io/v2/reference/news?published_utc=%s&apiKey=%s&limit=1000" %>%
    sprintf(yesterday, key) %>%
    hit_polygon(results_contain = 'published_utc')
  
  day_moves = day_moves$results%>%data.table
  today_news = today_news$results%>%data.table
  
  yesterday_news = yesterday_news$results%>%data.table
  yesterday_moves = yesterday_moves$results%>%data.table
  
  if(! 'T' %in% names(day_moves) || ! 'T' %in% names(yesterday_moves)){return(NULL)}
  
  combine_sources(day_moves, yesterday_moves, today_news, yesterday_news)
  }

days_to_look_at = as.Date(as.Date("2021-04-14"):Sys.Date())

# news_moves = sample(days_to_look_at, 20) %>%
#   lapply(get_day, key=POLYKEY) %>%
#   rbindlist(use.names=TRUE, fill=T)

news_moves = parallel::mclapply(
  days_to_look_at, 
                                get_day, key=POLYKEY
                               ,mc.cores = 16
                                ) %>% 
  rbindlist(use.names=TRUE, fill=T)
financials = stock_deets_v(POLYKEY, news_moves$ticker, 16)
news_moves = merge(news_moves,financials, by.x='ticker', by.y='symbol')

byword = news_moves[!sapply(keywords, is.null),
                  .(keywords=unlist(keywords) ), 
                  .(id, delta=c/o, overnight_delta=o/prev_close, ticker, single_ticker, marketcap, date, sector,publisher.name)]
byword[log(marketcap)<21 & !is.na(single_ticker)  & abs(overnight_delta-1)<.1,
       .(mean(delta,na.rm=T),
         median(delta,na.rm=T),
         length(unique(paste(date,ticker)))),.(keywords, month(date) )][order(month,decreasing = T)][keywords%in%c('Health', 'Penny Stocks')]
# short penny stocks with GlobeNewswire's "Health" or Benzinga's "Penny Stocks" keywords (single ticker)

byword[keywords %in% c('investing', 'Movers') & overnight_delta>1.02 & !is.na(single_ticker),
       .(mean(delta,na.rm=T),
         median(delta,na.rm=T),
         length(unique(paste(date,ticker)))),.(month(date))][order(month)]
# Long Motley Fool's investing and Benzinga's movers keywords with OTH increases 2-10% (single ticker)

byword[publisher.name=='PennyStocks' & log(marketcap)<21 & abs(overnight_delta-1)<.1,
       .(mean(delta,na.rm=T),
         median(delta,na.rm=T),
         length(unique(paste(date,ticker)))),.(publisher.name, month(date))][order(V1,decreasing = T)][V3>200]
# Long PennyStocks' penny stocks that didn't change too much from previous day

news_moves[grepl('Value', title, ignore.case=T),
       .(round(mean(c/o,na.rm=T),3),
         median(c/o,na.rm=T),
         length(unique(paste(date,ticker)))),.(publisher.name)][order(V3,decreasing = T)][1:20]
