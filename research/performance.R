drawdown = function(x){
  Reduce(function(.x,.y){
    if((.x+.y)>0){0} else {.x+.y}
  },
  x,0, accumulate=T)[-1]
}

performance=function(date,outcome,days_held,symbol){
  results = data.table(date=date,outcome=outcome,days_held=days_held,symbol=symbol)
  results=na.omit(results)
  results_daily = results[,.(outcome=sum(outcome,na.rm =T),trades=.N),date]
  setorder(results_daily, date)
  results_daily[,drawdown:=drawdown(outcome)]
  results_daily[,drawdown_i:=cumsum(drawdown==0)]
  results_overall= results_daily[,.(
    average=round(sum(outcome)/sum(trades),3),
    drawdown=round(min(drawdown,na.rm=T),1),
    total=round(sum(outcome),1),
    trades=sum(trades),
    days_traded=.N
  ), 
  year(date) ]%>%
    merge(results[,.(avg_days_held=mean(days_held),
                     stocks_traded=length(unique(symbol))),
                  year(date)])
  with(results_daily, plot(date,drawdown,type='l', 
                           ylim=c(-10,sum(results_daily$outcome ))))
  with(results_daily, points(date,cumsum(outcome),type='l'))
  abline(v = seq(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"),lty=2,col='gray')
  max_drawdown_days = max(results_daily[,.(datediff=max(date)-min(date)),drawdown_i][,datediff])
  print(results_overall[,.(perf=mean(average), 
                           drawdown=min(drawdown), 
                           drawdown_days=max(max_drawdown_days),
                           days_traded=sum(days_traded) )])
  results_overall[order(year)]
}