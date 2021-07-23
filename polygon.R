hit_polygon = function(link, tries = 3,results_contain=F){
  response = 'none'
  while(tries>0){
    response = tryCatch({
      x = link %>%jsonlite::fromJSON()
      stopifnot(x$response$status=='OK') 
      if(results_contain!=F){
        stopifnot(results_contain %in% response$results)
      }
      return(x)},
      error = {function(x){
        tries = tries-1
        'none'}})
  }
  return(response)
}

select_field = function(response, field){
    return(response[['results']][[field]])
}

financials_from_polygon = function( key, stockname, date, field=F){
  response = "https://api.polygon.io/v2/reference/financials/%s?type=Y&sort=-reportPeriod&apiKey=%s" %>%
    sprintf(stockname, key) %>%
    hit_polygon
  results = data.table(response$results)
  if('dateKey' %in% names(results)){
    results[dateKey<date][order(dateKey, decreasing = T)][1,]
  } else {
      return(data.table(ticker=stockname))
    }
}


stocklist_from_polygon = function(key, exchange = c('XNYS','XNAS'), date = '2018-01-01', financials=F){
  resultlist=list()
  for (ex in exchange){
    go=T
    last_examined=""
    while(go){
      response = "https://api.polygon.io/v3/reference/tickers?market=stocks&exchange=%s&date=%s&active=true&sort=ticker&order=asc&limit=1000&apiKey=%s&ticker.gt=%s" %>%
        sprintf(ex, date, key, last_examined) %>%
        hit_polygon
      if (!is.null(response$results)){
        last_examined = response$results$ticker[nrow(response$results)]
        resultlist[paste(ex, last_examined)]= list(response$results)
        go=nrow(response$results)==1000
      } else {
        go=F
      }
    }
  }
  out = resultlist %>% 
    rbindlist(use.names=TRUE, fill = T)
  if(financials){
    out$ticker %>% unlist %>%
      unique %>%
      parallel::mclapply(financials_from_polygon, key=key, date=date, field=F, mc.cores = 16) %>%
      rbindlist(fill=TRUE, use.names = T) %>%
      cbind(out) %>%
      return
  } else {
    return(out)
  }
} 

ticker_info_from_polygon = function( key, stockname, date, field=F) {
  "https://api.polygon.io/vX/reference/tickers/%s?date=%s&apiKey=%s" %>%
    sprintf(stockname, date, key) %>%
    hit_polygon %>%
    select_field(field=field)
}

stock_history = function(stockname, start_date, end_date, key, print=F, check_ticker=T){
  if(check_ticker){
    start_cik = end_cik = 'none'
    while(start_cik == 'none'){
      start_cik = ticker_info_from_polygon(key, stockname, start_date, field = 'cik')
      start_date = start_date+7
    }
    
    while((start_cik != end_cik) & (end_date>start_date+360)){
      end_cik = ticker_info_from_polygon(key, stockname, end_date, field = 'cik')
      end_date = end_date-7
    }
  }
  tries=0
  while(tries<3){
    response = "https://api.polygon.io/v2/aggs/ticker/%s/range/1/day/%s/%s?adjusted=true&sort=asc&apiKey=%s" %>%
      sprintf(stockname, start_date, end_date, key) %>%
      jsonlite::fromJSON() %>%
      hit_polygon
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
    rbindlist(fill=TRUE, use.names = T)
}
# stock_history('AI', as.Date('2017-01-01'),as.Date('2020-01-01'),POLYKEY)
