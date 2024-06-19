drawdown = function(x){
  Reduce(function(.x,.y){
    if((.x+.y)>0){0} else {.x+.y}
  },
  x,0, accumulate=T)[-1]
}

performance=function(date,outcome,days_held,symbol,sell_date=date, no_doubling=F, hold_less_than=F){
  results = data.table(date=date,outcome=outcome,days_held=days_held,symbol=symbol,sell_date=sell_date)
  results=na.omit(results)
  if(no_doubling){
    results=no_doubling(results)
  }
  if(hold_less_than){
    results[,to_include:=T]
    currently_held=c()
    for(da in sort(unique(results$date))  ){
      if(length(currently_held)<hold_less_than){
        currently_held=c(currently_held,results[date==da,sell_date])
      } else {
        results[date==da,to_include:=F]
      }
      currently_held=currently_held[currently_held>da]
    }
    results=results[to_include==T]
    results[,to_include:=NULL]
  }
  results_daily = results[,.(outcome=sum(outcome,na.rm =T),trades=.N),date]
  results_daily = merge(results_daily, results[,.(n_sold=.N),sell_date], 
                        all=T, by.x='date',by.y='sell_date')
  results_daily[,n_sold:=ifelse(is.na(n_sold),0,n_sold)]
  results_daily[,outcome:=ifelse(is.na(outcome),0,outcome)]
  results_daily[,trades:=ifelse(is.na(trades),0,trades)]
  setorder(results_daily, date)
  results_daily[,drawdown:=drawdown(outcome)]
  results_daily[,drawdown_i:=cumsum(drawdown==0)]
  results_daily[,n_held:=cumsum(trades-n_sold)]
  results_overall= results_daily[,.(
    average=round(sum(outcome)/sum(trades),3),
    drawdown=round(min(drawdown,na.rm=T),1),
    total=round(sum(outcome),1),
    trades=sum(trades),
    days_traded=length(unique(ifelse(trades>0,date,NA))),
    max_held=max(n_held)
  ), 
  year(date) ]%>%
    merge(results[,.(avg_days_held=mean(days_held),
                     stocks_traded=length(unique(symbol))),
                  year(date)])
  with(results_daily, plot(date,drawdown,type='l', 
                           ylim=c(-10,max(cumsum(outcome) )),
                           col='orange'))
  with(results_daily, points(date,cumsum(outcome),type='l'))
  with(results_daily, points(date,n_held/5,type='l',col="blue"))
  abline(v = seq(as.Date("2000-01-01"), as.Date("2030-12-31"), by = "year"),lty=2,col='gray')
  abline(v = seq(as.Date("2000-01-01"), as.Date("2030-12-31"), by = "month"),lty=3,col='lightgray')
  max_drawdown_days = max(results_daily[,.(datediff=max(date)-min(date)),drawdown_i][,datediff])
  print(results_overall[,.(avg_year=mean(average), 
                           avg_trade=sum(total)/sum(trades),
                           drawdown=min(drawdown), 
                           drawdown_days=max(max_drawdown_days),
                           days_traded=sum(days_traded),
                           max_held=max(max_held))])
  print(results_overall[order(year)])
  return(results_daily[,.(date,n_held)])
}

no_doubling=function(trades){
  setorder(trades, symbol, date)
  trades[,prev_sold:=shift(sell_date, n = 1, type = "lag"),symbol]
  setorder(trades, date, symbol)
  trades[is.na(prev_sold) | prev_sold<date]
}

performance_features(dataset){
  dataset[,lead1sell_rally:= shift(sell_rally,1,type='lead'),symbol]
  dataset[,lead1sell_rallydate:= shift(sell_rally_date,1,type='lead'),symbol]
  
  rally(dataset,
        sell_rule=function(dat){dat$lag1close<dat$lag1low+.2*(dat$lag1high-dat$lag1low) },
        varname='sell_lowclose',
        sell_close=F)
  dataset[,lead1sell_lowclose:= shift(sell_lowclose,1,type='lead'),symbol]
  dataset[,lead1sell_lowclosedate:= shift(sell_lowclose_date,1,type='lead'),symbol]
}