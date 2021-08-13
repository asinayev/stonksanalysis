library(data.table)

fillna = function(x, fill){
  ifelse(is.na(x),fill,x)
}
pct_diff = function(x,y,of=x){(x-y)/of}

basic_prep = function(indat,
                      rename_from = "AdjClose",
                      rename_to = "AdjClose",
                      end_date = 0,
                      start_date = 0){
  setnames(indat, rename_from, rename_to, skip_absent=TRUE)
  indat$Date=as.Date(indat$Date)
  all_combinations = expand.grid(
    Date = seq(min(indat$Date, na.rm = T),
               max(indat$Date, na.rm = T),1 ),
    stock = unique(indat$stock)
  )
  indat=data.table(indat)[all_combinations, on = c('Date','stock')]
  if (end_date!=0){
    indat=indat[Date<=end_date]
  }
  if (start_date!=0){
    indat=indat[Date>=start_date]
  }
  indat[order(stock,Date)]
  indat[,AdjCloseFilled:=AdjClose[1], .(cumsum(!is.na(AdjClose)),stock)]
  indat[,hiFilled:=high[1], .(cumsum(!is.na(high)),stock)]
  indat[,loFilled:=low[1], .(cumsum(!is.na(low)),stock)]
  indat[,atr:=pmax(abs(high-low),abs(high-shift(AdjCloseFilled)),abs(low-shift(AdjCloseFilled))), .(stock)]
  indat[,atr:=frollmean(shift(atr,1), 14, algo = 'exact',align='right',na.rm=T), stock]
  indat[,rsi:=1-(1/(1+frollmean(pmax(0, AdjClose - shift(AdjClose)), 14, algo = 'exact',align='right',na.rm=T)/
                      frollmean(pmax(0, shift(AdjClose) - AdjClose), 14, algo = 'exact',align='right',na.rm=T) 
                    )), stock ]
  indat
}

filter_range = function(fulldat, company_dates){
  fulldat[,stockdate:=Date]
  validrange = fulldat[company_dates, 
                       .(stock, Date=stockdate, valid=TRUE),
                       on=.(stock==ticker, Date>=ticker_valid_start, Date<ticker_valid_end)]
  fulldat = merge(fulldat, validrange, all.x=T, on=c('stock','Date')) 
  fulldat[,valid:=!is.na(valid)]
  fulldat=fulldat[,minValid:=min(ifelse(valid,Date,NA),na.rm=T)-365*2,stock]
  fulldat=fulldat[,maxValid:=max(ifelse(valid,Date,NA),na.rm=T)+365,stock]
  fulldat[Date>=minValid & Date<=maxValid,
          .(stock,Date,AdjClose,high,low,volume,AdjCloseFilled,hiFilled,loFilled,atr, rsi, valid)]
}
