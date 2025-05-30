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

get_all_results = function(link,results_contain=F){
  response = hit_polygon(link,results_contain = F)
  if(!'results' %in% names(response)){return(NULL)}
  results=response$results
  while('next_url' %in% names(response)){
    response=hit_polygon(response$next_url,results_contain = F)
    if(!'results' %in% names(response)){break}
    results=rbind(results,response$results)
  }
  return(results)
}

get_table_results = function(link, handle, tries=3, results_contain=F){
  counter = response = tries
  while(!is.data.table(response) && counter>0){
    counter = tryCatch({
      response=link %>%curl::curl(handle = handle)%>%read.csv%>%data.table
      stopifnot( is.data.table(response)  )
      if(results_contain!=F){
        stopifnot(results_contain %in% names(response))
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

stock_deets = function( key, stockname, date){
  x = "https://api.polygon.io/v3/reference/tickers/%s?date=%s&apiKey=%s" %>%
    sprintf(stockname, date, key) %>%
    hit_polygon
  if(!'results' %in% names(x)){return(NULL)}
  x$results[names(x$results)%in%c('ticker','name','market_cap','list_date','locale','total_employees','sic_description','description','cik')]
}

stock_deets_v = function(key, stocknames, cores, date){
  stocknames %>%
    unique %>%
    parallel::mclapply(stock_deets, key=key, mc.cores = cores, date=date) %>%
    rbindlist(fill=T, use.names = T)
}

financials_from_polygon = function( key, identifier, identifier_type='cik', field=F){
  if(identifier_type=='symbol'){
    out=
      "https://api.polygon.io/vX/reference/financials?ticker=%s&limit=100&sort=period_of_report_date&order=asc&apiKey=%s" %>%
      sprintf(identifier, key) %>%
      hit_polygon(results_contain = field)
  }
  if(identifier_type=='cik'){
    out=
      "https://api.polygon.io/vX/reference/financials?cik=%s&limit=100&sort=period_of_report_date&order=asc&apiKey=%s" %>%
      sprintf(identifier, key) %>%
      hit_polygon(results_contain = field)
  }
  out[[identifier_type]]=identifier
  return(out)
}

stocklist_from_polygon = function(key, date = '2018-01-01', details=F, cores=16, ticker_type='CS'){
  resultlist=list()
  go=T
  last_examined=""
  h=curl::new_handle()
  curl::handle_setheaders(h, "Accept"= "text/csv")
  if (ticker_type=='fx'){
    ticker_type_condition=paste('market',ticker_type,sep='=')
  } else {
    ticker_type_condition=paste('type',ticker_type,sep='=')
  }
  while(length(go)>0 && go){
    link = "https://api.polygon.io/v3/reference/tickers?date=%s&sort=ticker&order=asc&limit=1000&apiKey=%s&ticker.gt=%s&%s" %>%
      sprintf(date, key, sub(":",replacement = "%3A",x = last_examined),ticker_type_condition)
    response = get_table_results(link, handle=h)
    if (is.data.table(response) && nrow(response)>0){
      last_examined = response[.N,ticker]
      resultlist[[last_examined]]= response
      go=nrow(response)==1000
    } else {
      go=F
    }
  }
  
  out = resultlist %>%
    rbindlist(use.names=TRUE, fill = T)
  out = out[,.SD[.N],ticker]
  if(details) {
    out$ticker %>%
      stock_deets_v(key=key, cores=cores, date=date) %>%
      merge(out[,.(ticker)], by='ticker', all.y=T) %>%
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


stock_splits = function( key, stockname) {
  out="https://api.polygon.io/v3/reference/splits?ticker=%s&order=asc&limit=1000&sort=execution_date&apiKey=%s" %>%
    sprintf(stockname, key) %>%
    hit_polygon
  out$results
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
  dcast(stockdat, stock+bar_date~bar_hour, value.var = c('AdjClose','Open'), fun.aggregate=mean) %>%
    return
}

sampled_data=function(key, date, end_date = as.Date(date)+365,
                      ticker_type=c('CS'),details=F){
  stocks = stocklist_from_polygon(key = key, date = date, ticker_type=ticker_type,details = details)
  stocklist = parallel::mclapply(stocks$ticker, stock_history,
                                 start_date = as.Date(date),
                                 end_date = end_date,
                                 key = key,
                                 mc.cores = 16, check_ticker=F)
  if(details){
    stocklist[unlist(lapply(stocklist,is.data.frame))] %>%
      rbindlist(fill=TRUE, use.names = T) %>%
      merge(stocks, by.x='stock', by.y='ticker')%>%
      return
  } else {
    stocklist[unlist(lapply(stocklist,is.data.frame))] %>%
      rbindlist(fill=TRUE, use.names = T) %>%
      return
  }
}

get_prev_day_news = function(date, key, full_prevday=T, apply_=F){
  
  if(!lubridate::wday(date) %between% c(2,6)){
    return(NULL)
  } else if (lubridate::wday(date)==2){
    yesterday = date-3
  } else {
    yesterday = date-1
  }
  if(full_prevday){
    open = lubridate::as_datetime(paste(yesterday,"12:00:00",collapse = "T"),tz='America/New_York')
  } else {
    open = lubridate::as_datetime(paste(yesterday,"16:00:00",collapse = "T"),tz='America/New_York')
  }
  if(apply_){
    close = lubridate::as_datetime(paste(date,"09:30:00",collapse = "T"),tz='America/New_York')
  } else {
    close = lubridate::as_datetime(paste(date,"07:30:00",collapse = "T"),tz='America/New_York')
  }
  today_news = "https://api.polygon.io/v2/reference/news?published_utc.gt=%s&published_utc.lt=%s&apiKey=%s&limit=1000" %>%
    sprintf(open %>% lubridate::with_tz('UTC') %>% format("%Y-%m-%dT%H:%M:%SZ"),
            close %>% lubridate::with_tz('UTC') %>% format("%Y-%m-%dT%H:%M:%SZ"), key) %>%
    get_all_results(results_contain = 'published_utc')
  if(!all(c('id', 'publisher', "published_utc", 'title', 'author', 'tickers') %in% names(today_news)) ){
    return(NULL)
  }
  today_news$date=yesterday
  today_news%>%data.table
}

get_financials = function(stocks,id_type='cik', key=POLYKEY){
  financials = parallel::mclapply(unlist(unique(stocks[,..id_type])),
                                  financials_from_polygon,
                                  identifier_type=id_type,
                                  key=key, field=F,
                                  mc.cores = 16)
  process_recordset=function(finrec){
    if(!is.null(finrec$results$financials$income_statement$diluted_earnings_per_share$value)&
       !is.null(finrec$results$financials$income_statement$basic_earnings_per_share$unit)&
       !is.null(finrec$results$financials$income_statement$diluted_average_shares$value)&
       !is.null(finrec$results$financials$cash_flow_statement$net_cash_flow_from_operating_activities$value)&
       !is.null(finrec$results$financials$balance_sheet$current_liabilities$value)&
       !is.null(finrec$results$financials$balance_sheet$current_assets$value)){
      fins = data.table(identifier=finrec[[id_type]],
                        start_date=finrec$results$start_date,
                        end_date=finrec$results$end_date,
                        filing_date=as.Date(finrec$results$filing_date),
                        timeframe=finrec$results$timeframe,
                        eps_unit=finrec$results$financials$income_statement$basic_earnings_per_share$unit,
                        eps_value=finrec$results$financials$income_statement$basic_earnings_per_share$value,
                        deps_value=finrec$results$financials$income_statement$diluted_earnings_per_share$value,
                        operating_cash=finrec$results$financials$cash_flow_statement$net_cash_flow_from_operating_activities$value,
                        liabilities=finrec$results$financials$balance_sheet$current_liabilities$value,
                        assets=finrec$results$financials$balance_sheet$current_assets$value,
                        shares=finrec$results$financials$income_statement$diluted_average_shares$value)
      return(fins[timeframe=='quarterly'][order(filing_date),
                                          .(deps_value=deps_value[.N],eps_unit=eps_unit[.N],eps_value=eps_value[.N],operating_cash=operating_cash[.N],liabilities=liabilities[.N],assets=assets[.N],shares=shares[.N],filing_date=filing_date[.N]),
                                          .(identifier,start_date,end_date)])
    } else {
      return(
        data.frame(identifier="",start_date="",end_date="",filing_date=as.Date(0),deps_value=0,eps_unit='',eps_value=0,operating_cash=0,liabilities=0,assets=0,shares=0)
      )
    }
  }
  financials = financials[unlist(lapply(financials,function(x)is.data.frame(x$results) ))] %>%
    lapply(process_recordset)%>%
    rbindlist(fill=TRUE, use.names = T)
  stocks[,joining_date:=date]
  stocks[,identifier:=get(id_type)]
  stock_cols = c(names(stocks),"deps_value","eps_value","eps_unit","operating_cash","liabilities","assets","shares",
                 'filing_date','end_date')
  financials[,joining_filing_date:=filing_date]
  financials[,year_after_end_date:=as.Date(end_date)+365+90]
  gc()
  financials[stocks,
             ..stock_cols,
             on = .(identifier,
                    joining_filing_date<=joining_date,
                    year_after_end_date>=joining_date)
  ][,.(mean_deps=mean(deps_value),std_deps=sd(eps_value),mean_eps=mean(eps_value),mid_eps=median(eps_value),mid_deps=median(deps_value),
       mean_cash=mean(operating_cash),eps_unit=eps_unit[1],
       liabilities=mean(liabilities),assets=mean(assets),
       shares=mean(shares)),
    names(stocks)]
}



