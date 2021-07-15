stocklist_from_polygon = function(key, exchange = 'XNYS', date = '2018-01-01'){
  go=T
  resultlist=list()
  last_examined=""
  while(go){
    response = "https://api.polygon.io/v3/reference/tickers?type=CS&market=stocks&exchange=%s&date=%s&active=true&sort=ticker&order=asc&limit=1000&apiKey=%s&ticker.gt=%s" %>%
      sprintf(exchange, date, , last_examined) %>%
      jsonlite::fromJSON()
    last_examined = response$results[nrow(response$results),]$ticker
    resultlist[last_examined]=response$results
    go=nrow(response$results)==1000
  }
  resultlist %>% rbindlist
}

stocklist = stocklist_from_polygon()
