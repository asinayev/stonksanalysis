stocklist_from_polygon = function(key, exchange = c('XNYS','XNAS'), date = '2018-01-01', type='CS'){
  resultlist=list()
  for (ex in exchange){
    go=T
    last_examined=""
    while(go){
      response = "https://api.polygon.io/v3/reference/tickers?type=%s&market=stocks&exchange=%s&date=%s&active=true&sort=ticker&order=asc&limit=1000&apiKey=%s&ticker.gt=%s" %>%
        sprintf(type, ex, date, key, last_examined) %>%
        jsonlite::fromJSON()
      if (!is.null(response$results)){
        last_examined = response$results$ticker[nrow(response$results)]
        resultlist[paste(ex, last_examined)]= list(response$results)
        go=nrow(response$results)==1000
      } else {
        go=F
      }
    }
  }
  resultlist %>% rbindlist(use.names=TRUE)
}

ticker_info_from_polygon = function( key, stockname, date ) {
    "https://api.polygon.io/vX/reference/tickers/%s?date=%s&apiKey=%s" %>%
      sprintf(stockname, date, key) %>%
      jsonlite::fromJSON()
}

stock_history = function(stockname, start_date, end_date, key, print=F){
  description_start = description_end = list(results=list(cik='none'))
  
  while((description_start$results$cik != 'none')){
    description_start = tryCatch({
      ticker_info_from_polygon(key, stockname, start_date)},
      error = {function(x){
        start_date = start_date+7
        description_start}})
  }
  
  while((description_start$results$cik != description_end$results$cik) & (end_date>start_date+360)){
    end_date = end_date-7
    description_end = tryCatch({
      ticker_info_from_polygon(key, stockname, end_date)},
      error = {function(x){
        end_date = end_date-7
        description_end}})
  }
  
  tries=0
  while(tries<10){
    response = "https://api.polygon.io/v2/aggs/ticker/%s/range/1/day/%s/%s?adjusted=true&sort=asc&apiKey=%s" %>%
      sprintf(stockname, start_date, end_date, key) %>%
      jsonlite::fromJSON()
    tries = tries+1
    if('results' %in% names(response) & 'c' %in% names(response$results)){
      return(
        data.table(stock = stockname,
                   AdjClose = response$results$c, 
                   volume = response$results$v, 
                   Date= (response$results$t/1000) %>% as.POSIXct(origin="1970-01-01", tz = 'New York') %>% as.Date() )
      )
    }
  }
  return(
    data.table(stock = stockname,
               AdjClose = NA, 
               volume = NA, 
               Date= as.Date(start_date) )
  )
}

sampled_data=function(key, date, nsample, exchange = c('XNYS','XNAS','XASE')){
  stocklist = stocklist_from_polygon(key = key, date = date, exchange = exchange) %>%
    dplyr::select('ticker') %>% unlist %>%
    unique %>% sample(nsample)  %>%
    parallel::mclapply(stock_history,
                       start_date = date-365*2, 
                       end_date = date+365, 
                       key = key,
                       mc.cores = 16)
  stocklist[unlist(lapply(stocklist,is.data.frame))] %>%
    rbindlist
}
# stock_history('AI', as.Date('2017-01-01'),as.Date('2020-01-01'),POLYKEY)
