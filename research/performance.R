drawdown = function(x){
  Reduce(function(.x,.y){
    if((.x+.y)>0){0} else {.x+.y}
  },
  x,0, accumulate=T)[-1]
}

performance=function(date,outcome,days_held,symbol,sell_date=date){
  results = data.table(date=date,outcome=outcome,days_held=days_held,symbol=symbol,sell_date=sell_date)
  results=na.omit(results)
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
                           ylim=c(-10,max(cumsum(outcome) ))))
  with(results_daily, points(date,cumsum(outcome),type='l'))
  with(results_daily, points(date,n_held/5,type='l'))
  abline(v = seq(as.Date("2000-01-01"), as.Date("2030-12-31"), by = "year"),lty=2,col='gray')
  abline(v = seq(as.Date("2000-01-01"), as.Date("2030-12-31"), by = "month"),lty=3,col='lightgray')
  max_drawdown_days = max(results_daily[,.(datediff=max(date)-min(date)),drawdown_i][,datediff])
  print(results_overall[,.(avg_year=mean(average), 
                           avg_trade=sum(total)/sum(trades),
                           drawdown=min(drawdown), 
                           drawdown_days=max(max_drawdown_days),
                           days_traded=sum(days_traded),
                           max_held=max(max_held))])
  results_overall[order(year)]
}