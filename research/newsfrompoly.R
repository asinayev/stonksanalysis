get_day = function(date, key){
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
  yesterday_news[published_utc %>% as_datetime() %>% with_tz('America/New_York') %>% hour() >3]
  today_news[published_utc %>% as_datetime() %>% with_tz('America/New_York') %>% hour() <9]
  day_news = rbind(yesterday_news, today_news, fill=T)
  
  day_news[,single_ticker:=ifelse(lapply(tickers, length)==1, 
                                  unlist(lapply(tickers, function(x)x[[1]])), 
                                  NA)]
  day_news[,date:=date]
  
  merge(day_news[!is.na(single_ticker)], 
        day_moves, by.x='single_ticker', by.y='T', all.x=T) %>%
    merge(yesterday_moves[,.(`T`,prev_close = c)], 
          by.x='single_ticker', by.y='T', all.x=T)
  }

days_to_look_at = as.Date(as.Date("2021-04-14"):Sys.Date())

news_moves = parallel::mclapply(
  days_to_look_at, 
                                get_day, key=POLYKEY
                               ,mc.cores = 16
                                ) %>% 
  rbindlist(use.names=TRUE, fill=T)
financials = stock_deets_v(POLYKEY, news_moves$single_ticker, 16)
news_moves = merge(news_moves,financials, by.x='single_ticker', by.y='symbol')

byword = news_moves[!sapply(keywords, is.null),
                  .(keywords=unlist(keywords) ), 
                  .(id, delta=c/o, overnight_delta=o/prev_close, single_ticker, marketcap, date)]
byword[marketcap<1000000000 ,
       .(mean(delta,na.rm=T),
         median(delta,na.rm=T),
         length(unique(paste(date,single_ticker)))),keywords][order(V3,decreasing = T)][1:20]
# small caps with "Health keyword"

byword[marketcap>1000000000 & overnight_delta>1.02,
       .(mean(delta,na.rm=T),
         median(delta,na.rm=T),
         length(unique(paste(date,single_ticker)))),keywords][order(V3,decreasing = T)][1:20]
# investing and movers keywords with OTH increases