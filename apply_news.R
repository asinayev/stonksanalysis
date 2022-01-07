require(tidyquant, quietly = T)
require(data.table, quietly = T)

args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){args='~/stonksanalysis'}
setwd(args[1])
source("polygon.R", local=T)
POLYKEY = Sys.getenv('POLYGONKEY')

stopifnot(POLYKEY!='')

news_since_yesterday=function(apikey){
  current_news = "https://api.polygon.io/v2/reference/news?order=desc&sort=published_utc&apiKey=%s&limit=1000" %>%
    sprintf(apikey) %>%
    hit_polygon(results_contain = 'published_utc')
  
  current_news = current_news$results %>%data.table
  current_news[, published_nyc := published_utc%>% as_datetime() %>% with_tz('America/New_York')]
  
  last_close = as_datetime(paste(Sys.Date()-1,'16:00:00', collapse='T'), tz='America/New_York')
  last_open = as_datetime(paste(Sys.Date(),'09:30:00', collapse='T'), tz='America/New_York')
  current_news[published_nyc>last_close & published_nyc<last_open]
}

enrich = function(stocklist, moves, apikey){
  stocklist=unlist(stocklist)
  financials = stock_deets_v(apikey, unique(stocklist), 8)
  financials[,symbol:=ticker]
  moves[,price:=lastTrade.p]
  moves[ticker %in% unique(stocklist)] %>%
    merge(financials, by.x='ticker', by.y='ticker')
}

matching_news = function(news, keyword, publisher, max_tickers=Inf){
  keyword_match = function(word, keyword_list){
    if(is.na(word)){
      return(T)
    } else {
      return(word %in% keyword_list)  
      }
    }
  x=news[sapply(keywords, keyword_match, word=keyword) & 
         publisher.name==publisher & 
         sapply(tickers, function(x)length(x)<=max_tickers), tickers]
  x
}

current_moves = "https://api.polygon.io/v2/snapshot/locale/us/markets/stocks/tickers?apiKey=%s" %>%
  sprintf(POLYKEY) %>%
  hit_polygon %>% data.table
current_news = news_since_yesterday(POLYKEY)

# short penny stocks with GlobeNewswire's "Health" or Benzinga's "Penny Stocks" keywords (single ticker)
matching_news(current_news, keyword='Health', publisher='GlobeNewswire Inc.', max_tickers=1) %>%
  enrich(current_moves$tickers, POLYKEY) %>%
  subset(log(market_cap)<21, select=c('symbol','price','prevDay.c')) %>%
  fwrite('/tmp/shortpenny.csv')
matching_news(current_news, keyword='Penny Stocks', publisher='Benzinga', max_tickers=1) %>%
  enrich(current_moves$tickers, POLYKEY) %>%
  subset(log(market_cap)<21, select=c('symbol','price','prevDay.c')) %>%
  fwrite('/tmp/shortpenny.csv', append = T)
# Long Motley Fool's investing and Benzinga's movers keywords with OTH increases 2-10% (single ticker)
matching_news(current_news, keyword='investing', publisher='The Motley Fool', max_tickers=1) %>%
  enrich(current_moves$tickers, POLYKEY) %>%
  subset(lastTrade.p > prevDay.c*1.02 & lastTrade.p < prevDay.c*1.1, 
         select=c('symbol','price','prevDay.c')) %>%
  fwrite('/tmp/longzing.csv')
matching_news(current_news, keyword='Movers', publisher='Benzinga', max_tickers=1) %>%
  enrich(current_moves$tickers, POLYKEY) %>%
  subset(lastTrade.p > prevDay.c*1.02 & lastTrade.p < prevDay.c*1.1, 
         select=c('symbol','price','prevDay.c')) %>%
  fwrite('/tmp/longzing.csv', append=T)
# Long PennyStocks' penny stocks that didn't change too much from previous day
matching_news(current_news, keyword=NA, publisher='PennyStocks', max_tickers=Inf) %>%
  enrich(current_moves$tickers, POLYKEY) %>%
  subset(lastTrade.p > prevDay.c*.9 & lastTrade.p < prevDay.c*1.1 & log(market_cap)<21, 
         select=c('symbol','price','prevDay.c')) %>%
  fwrite('/tmp/longpenny.csv')

