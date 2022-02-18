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
  second_page = hit_polygon(paste0(current_news$next_url,"&apiKey=",POLYKEY) )

  current_news = rbind(current_news$results %>%data.table, second_page$results %>%data.table)
  current_news[, published_nyc := published_utc%>% as_datetime() %>% with_tz('America/New_York')]
  
  yesterday_open = as_datetime(paste(Sys.Date()-1,'09:30:00', collapse='T'), tz='America/New_York')
  yesterday_close = as_datetime(paste(Sys.Date()-1,'16:00:00', collapse='T'), tz='America/New_York')
  today_open = as_datetime(paste(Sys.Date(),'09:30:00', collapse='T'), tz='America/New_York')
  current_news[,news_oth:=published_nyc>yesterday_close]
  current_news[published_nyc>yesterday_open & published_nyc<today_open]
}

enrich = function(stocklist, moves, apikey){
  if(length(stocklist)==0){
    stocklist='GOOG'
  }
  moves = data.table(moves$tickers)
  stocklist=unlist(stocklist)
  financials = stock_deets_v(apikey, unique(stocklist), 8)
  yahoo_results = tq_get(unique(stocklist), from=Sys.Date()-100) %>% data.table
  enriched_moves = moves[ticker %in% unique(stocklist)] %>%
    merge(financials, by.x='ticker', by.y='ticker') %>% 
    merge(yahoo_results[order(symbol,date), .(yahoo_price = last(close),yahoo_vol=last(volume)), symbol], 
          by.x='ticker',by.y='symbol', all.x=T)
  enriched_moves[,symbol:=ticker]
  enriched_moves[,c('price','lastTrade.p'):=ifelse(lastTrade.p!=0,lastTrade.p,yahoo_price) ]
  enriched_moves[,prevDay.c:=ifelse(prevDay.c!=0,prevDay.c,yahoo_price) ]
  enriched_moves[,volume:=ifelse(prevDay.v!=0,prevDay.v,yahoo_vol)]
  return(enriched_moves[symbol!='GOOG'])
}

matching_news = function(news, keyword, publisher, max_tickers=Inf, allow_yesterday=F){
  keyword_match = function(word, keyword_list){
    if(all(is.na(word))){
      return(T)
    } else {
      return(any(word %in% keyword_list))
      }
    }
  x=news[sapply(keywords, keyword_match, word=keyword) & 
           ifelse(news_oth,T,allow_yesterday) &
         publisher.name==publisher & 
         sapply(tickers, function(x)length(x)<=max_tickers), tickers]
  x
}

current_moves = "https://api.polygon.io/v2/snapshot/locale/us/markets/stocks/tickers?apiKey=%s" %>%
  sprintf(POLYKEY) %>%
  hit_polygon
current_news = news_since_yesterday(POLYKEY)

# short penny stocks with GlobeNewswire's "Health" 
matching_news(current_news, keyword=c('Health', 'Partnerships', 'Press releases'), 
              publisher='GlobeNewswire Inc.', max_tickers=Inf, allow_yesterday=F) %>%
  enrich(current_moves, POLYKEY) %>%
  subset(log(market_cap)<21 & volume*prevDay.c>75000, select=c('symbol','price','prevDay.c','volume')) %>%
  dplyr::mutate( action='SELL', close=prevDay.c, 
                 strike_price=round(prevDay.c*1.015,2), 
                 order_type='LMT', time_in_force='OPG') %>%
  fwrite('/tmp/healthshort.csv')
# Short PennyStocks' penny stocks with upward movement
matching_news(current_news, keyword=NA, publisher='PennyStocks', max_tickers=Inf, 
              allow_yesterday=T) %>%
  enrich(current_moves, POLYKEY) %>%
  subset(log(market_cap)<21 & volume*prevDay.c>75000, select=c('symbol','prevDay.c','volume')) %>%
  dplyr::mutate( action='SELL', close=prevDay.c, 
                 strike_price=round(prevDay.c*1.015,2), 
                 order_type='LMT', time_in_force='OPG') %>%
  fwrite('/tmp/pennyshort.csv')
# short Benzinga's penny stocks with Penny Stocks and Small Cap keywords and upward movement >1%
matching_news(current_news, keyword=c('Penny Stocks', 'Small Cap'), publisher='Benzinga', 
              max_tickers=Inf, allow_yesterday=T) %>%
  enrich(current_moves, POLYKEY) %>%
  subset(log(market_cap)<21 & volume*prevDay.c>75000, select=c('symbol','prevDay.c','volume')) %>%
  dplyr::mutate( action='SELL', close=prevDay.c, 
                 strike_price=round(prevDay.c*1.015,2), 
                 order_type='LMT', time_in_force='OPG') %>%
  fwrite('/tmp/zingashort.csv')

