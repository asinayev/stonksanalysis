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
  with(results_daily, plot(date,drawdown,type='l'))
  print(results_overall[,.(perf=mean(average), drawdown=min(drawdown), days_traded=sum(days_traded) )])
  results_overall[order(year)]
}