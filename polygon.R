stocklist_from_polygon = function(key, exchange = c('XNYS'), date = '2018-01-01'){
  resultlist=list()
  for (ex in exchange){
    go=T
    last_examined=""
    while(go){
      response = "https://api.polygon.io/v3/reference/tickers?type=CS&market=stocks&exchange=%s&date=%s&active=true&sort=ticker&order=asc&limit=1000&apiKey=%s&ticker.gt=%s" %>%
        sprintf(ex, date, key, last_examined) %>%
        jsonlite::fromJSON()
      print(str(response$results))
      if (!is.null(response$results)){
        last_examined = response$results$ticker[nrow(response$results)]
        resultlist[paste(ex, last_examined)]= list(response$results)
        go=nrow(response$results)==1000
      } else {
        print(response)
        go=F
      }
    }
  }
  resultlist %>% rbindlist(use.names=TRUE)
}

stock_history = function(stockname, start_date, end_date, key, print=F){
  tries=0
  while(tries<10){
    response = "https://api.polygon.io/v2/aggs/ticker/%s/range/1/day/%s/%s?adjusted=true&sort=asc&apiKey=%s" %>%
      sprintf(stockname, start_date, end_date, key) %>%
      jsonlite::fromJSON()
    tries = tries+1
    if('results' %in% names(response) & 'c' %in% names(response$results)){
      go=10}
  }
  
  data.table(stock = stockname,
             AdjClose = response$results$c, 
             volume = response$results$v, 
             Date= (response$results$t/1000) %>% as.POSIXct(origin="1970-01-01", tz = 'New York') %>% as.Date() )
}

stock_history('AAC', '2017-01-01','2020-01-01',POLYKEY)
