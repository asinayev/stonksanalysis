require(tidyquant, quietly = T)
require(data.table, quietly = T)

source("polygon.R", local=T)
POLYKEY = Sys.getenv('POLYGONKEY')
out_dir = Sys.getenv('out_dir')

stopifnot(POLYKEY!='')

write_strat = function(strat_dat, strat_name, base_dir=out_dir){
  out_file_name=paste0(base_dir,"/",strat_name,".csv")
  fwrite(strat_dat,out_file_name)
}


performance=function(date,outcome,days_held,symbol){
  results = data.table(date=date,outcome=outcome,days_held=days_held,symbol=symbol)
  results=na.omit(results)
  results_daily = results[,.(outcome=sum(outcome),trades=.N),date]
  setorder(results_daily, date)
  results_daily[,trailing_sum:=frollsum(outcome ,n = 25, align='right',fill=NA)]
  results_overall= results_daily[,.(
    average=round(sum(outcome)/sum(trades),3),
    drawdown=round(min(trailing_sum,na.rm=T),1),
    total=round(sum(outcome),1),
    trades=sum(trades),
    days_traded=.N
  ), 
  year(date) ]%>%
    merge(results[,.(avg_days_held=mean(days_held),
                     stocks_traded=length(unique(symbol))),
                  year(date)])
  print(results_overall[,.(perf=mean(average), drawdown=min(drawdown), days_traded=sum(days_traded) )])
  results_overall[order(year)]
}
