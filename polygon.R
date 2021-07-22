stocklist_from_polygon = function(key, exchange = c('XNYS','XNAS'), date = '2018-01-01'){
  resultlist=list()
  for (ex in exchange){
    go=T
    last_examined=""
    while(go){
      response = "https://api.polygon.io/v3/reference/tickers?market=stocks&exchange=%s&date=%s&active=true&sort=ticker&order=asc&limit=1000&apiKey=%s&ticker.gt=%s" %>%
        sprintf(ex, date, key, last_examined) %>%
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
  resultlist %>% rbindlist(use.names=TRUE, fill = T)
}

ticker_info_from_polygon = function( key, stockname, date, field=F, wait=F) {
  response = 'none'
  tries = 3
  while(!is.list(response) & tries>0){
    response = tryCatch({
      "https://api.polygon.io/vX/reference/tickers/%s?date=%s&apiKey=%s" %>%
        sprintf(stockname, date, key) %>%
        jsonlite::fromJSON()},
      error = {function(x){
        tries = tries-1
        'none'}})
  }
  if(wait!=F){
    Sys.sleep(wait)
  }
  if(field==F){
    return(response)
  } else {
    return(response[['results']][[field]])
  }
}

stock_history = function(stockname, start_date, end_date, key, print=F, check_ticker=T){
  if(check_ticker){
    start_cik = end_cik = 'none'
    while(start_cik == 'none'){
      start_cik = tryCatch({
        ticker_info_from_polygon(key, stockname, start_date, field = 'cik')},
        error = {function(x){
          start_date = start_date+7
          start_cik}})
    }
    
    while((start_cik != end_cik) & (end_date>start_date+360)){
      end_date = end_date-7
      end_cik = tryCatch({
        ticker_info_from_polygon(key, stockname, end_date, field = 'cik')},
        error = {function(x){
          end_date = end_date-7
          end_cik}})
    }
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
                   high = response$results$h,
                   low = response$results$l,
                   volume = response$results$v, 
                   Date= (response$results$t/1000) %>% as.POSIXct(origin="1970-01-01", tz = 'New York') %>% as.Date() )
      )
    }
  }
  return(
    data.table(stock = stockname,
               AdjClose = NA, 
               high = NA, 
               low = NA, 
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
