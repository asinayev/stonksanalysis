hit_polygon = function(link, tries = 3,results_contain=F){
  counter = response = tries
  while(is(response, 'numeric') && counter>0){
    counter = tryCatch({
        response = link %>%jsonlite::fromJSON()
        stopifnot( response$status %in% c('OK', "DELAYED")  )
        if(results_contain!=F){
          stopifnot(results_contain %in% names(response$results))
        }
        return(response)
      },
      error = function(error){
        return(counter-1) }
    )
  }
  return(counter)
}

select_field = function(response, field){
    return(response[['results']][[field]])
}

stock_deets = function( key, stockname){
  x = "https://api.polygon.io/vX/reference/tickers/%s?apiKey=%s" %>%
    sprintf(stockname, key) %>%
    hit_polygon
  if(!'results' %in% names(x)){return(NULL)}
  x$results[names(x$results)%in%c('ticker','name','market_cap','list_date','locale','total_employees')] 
}

stock_deets_v = function(key, stocknames, cores){
  stocknames %>%
    unique %>%
    parallel::mclapply(stock_deets, key=key, mc.cores = cores) %>% 
    rbindlist(fill=T, use.names = T)
}

financials_from_polygon = function( key, stockname, date, field=F){
  response = "https://api.polygon.io/v2/reference/financials/%s?type=Y&sort=-reportPeriod&apiKey=%s" %>%
    sprintf(stockname, key) %>%
    hit_polygon
  if('results' %in% names(response) && 'dateKey' %in% names(response$results)){
    results = data.table(response$results)
    results[dateKey<date][order(dateKey, decreasing = T)][1,]
  } else {
      return(data.table(ticker=stockname))
    }
}


stocklist_from_polygon = function(key, exchange = c('XNYS','XNAS'), date = '2018-01-01', financials=F, cores=16){
  resultlist=list()
  for (ex in exchange){
    go=T
    last_examined=""
    while(go){
      link = "https://api.polygon.io/v3/reference/tickers?market=stocks&exchange=%s&date=%s&active=true&sort=ticker&order=asc&limit=1000&apiKey=%s&ticker.gt=%s" %>%
        sprintf(ex, date, key, last_examined)
      response = hit_polygon(link)
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
  out = out[,.SD[.N],ticker]
  if(financials){
    out$ticker %>% unlist %>%
      unique %>%
      parallel::mclapply(financials_from_polygon, key=key, date=date, field=F, mc.cores = cores) %>%
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
  link = "https://api.polygon.io/v2/aggs/ticker/%s/range/1/day/%s/%s?adjusted=true&sort=asc&limit=50000&apiKey=%s" %>%
    sprintf(stockname, start_date, end_date, key)
  response = hit_polygon(link, tries = 3, results_contain = "c")
  if (!is(response, 'numeric')){
    return(
      data.table(stock = stockname,
                 AdjClose = response$results$c, 
                 open = response$results$o,
                 high = response$results$h,
                 low = response$results$l,
                 volume = response$results$v, 
                 Date= (response$results$t/1000) %>% as.POSIXct(origin="1970-01-01", tz = 'New York') %>% as.Date() )
    )
  } else {
    return(
      data.table(stock = stockname,
                 AdjClose = NA, 
                 open = NA,
                 high = NA, 
                 low = NA, 
                 volume = NA, 
                 Date= as.Date(start_date) )
    )
  }
}

add_results = function(results, ...){
  
  response = "https://api.polygon.io/v2/aggs/ticker/%s/range/%s/%s/%s/%s?adjusted=false&sort=asc&limit=50000&apiKey=%s" %>%
    sprintf(...) %>%
    hit_polygon(tries = 3, results_contain = "c")
  if('results' %in% names(response)){
    rbind(results,
          data.table(stock = response$ticker,
                     Open = response$results$o, 
                     AdjClose = response$results$c, 
                     high = response$results$h,
                     low = response$results$l,
                     volume = response$results$v, 
                     TimeStamp = response$results$t,
                     DateTime= (response$results$t/1000) %>% as.POSIXct(origin="1970-01-01", tz = 'EST') 
          ),
          fill=TRUE
    ) %>% return
  } else {
      return(results)
    }
}

stock_day = function(stockname, start_date, end_date, key, 
                     interval='minute', interval_len=1, day_buffer = 1){
  results = data.table(stock = stockname,
                       AdjClose = 0, 
                       high = 0, 
                       low = 0, 
                       volume = 0, 
                       TimeStamp = 0,
                       DateTime= as.POSIXct(as.Date(start_date), tz = 'EST'))[stock==1]
  results = add_results(results, stockname, interval_len, interval, 
            start_date, end_date, key)
  while(nrow(results)>=1 & 
        as_date(end_date) - as_date(max(results$DateTime)) > day_buffer){
    new_results = add_results(results, stockname, interval_len, interval, 
                as_date(max(results$DateTime)), end_date, key)
    if(new_results[,max(DateTime)]>results[,max(DateTime)]){
      results=new_results
    } else { break }
  }
  return(unique(results,by=c('stock','TimeStamp')))
}

get_hours_for_stocks = function(stocknames,
                                start_date='2018-01-01', 
                                end_date=Sys.Date(),
                                key=POLYKEY){
  stockdat = 
    stocknames %>%
    unique %>%
    parallel::mclapply(
      stock_day,
      start_date=start_date,
      end_date=end_date,
      key=key,
      interval='minute',
      interval_len=60, day_buffer = 7,
      mc.cores = 20) %>% 
    rbindlist(fill = T)
  setorder(stockdat, stock, DateTime)
  stockdat[, DateTime := as.POSIXct(TimeStamp/1000, 
                                    origin="1970-01-01", tz = 'EST')]
  stockdat[,bar_date:=as_date(DateTime)]
  stockdat[,bar_hour:=lubridate::hour(DateTime)]
  dcast(stockdat, stock+bar_date~bar_hour, value.var = c('AdjClose','Open')) %>%
    return
}

sampled_data=function(key, date, end_date = as.Date(date)+365, nsample, exchange = c('XNYS','XNAS','XASE')){
  stocks = stocklist_from_polygon(key = key, date = date, exchange = exchange) %>%
    subset(type=='CS') %>%
    dplyr::select('ticker') %>% unlist %>%
    unique
  if(nsample){
    stocks=sample(stocks, nsample)
  }
  stocklist = parallel::mclapply(stocks, stock_history,
                       start_date = as.Date(date), 
                       end_date = end_date, 
                       key = key,
                       mc.cores = 16, check_ticker=F)
  stocklist[unlist(lapply(stocklist,is.data.frame))] %>%
    rbindlist(fill=TRUE, use.names = T)
}




