get_tech = function(price_dat){
  setorder(price_dat,low)
  dates = price_dat$date[1:3]
  vals = price_dat$low[1:3]
  setorder(price_dat,-high)
  dates = c(dates, price_dat$date[1:3])
  vals = c(vals, price_dat$high[1:3])
  return(list(dates=dates, vals=vals))
}

dbl_bottom = function(price_dat){
  price_dat=data.table(price_dat)
  techs = get_tech(price_dat)
  today = price_dat[.N]
  any(today$date==techs$dates[1:2]+2) & 
    any((techs$dates[4:6]) %between% (techs$dates[2:1]))
}

prices[1:10000][,is_reversal:=zoo::rollapply(data=.SD[,.(high,low,date=as.integer(date))],
                                             FUN=dbl_bottom,width=25, 
                                             by.column = FALSE,fill=NA),
                symbol]