strategy = function(params, allstocks=allstocks, stocknames=stocknames){
  # print(params)
  x = lapply(stocknames,function(stockname){
    x=allstocks[stock==stockname]
    x[,stock:=stockname]
    if(length(unique(x[sample=='train']$AdjClose))>100){
      model = lm(CloseDiff~CloseDiffLag1*CloseDiff7D+CloseDiffLag2+CloseDiffLag3,
                 data=x, subset=sample=="train")
      x[,PredDiff:=predict(model,x)]
      error_improvement = pct_diff(x[sample=='test',mean(abs(CloseDiff-mean(CloseDiff)))], x[sample=='test',mean(abs(PredDiff-CloseDiff))])
      if(!is.na(error_improvement) & (error_improvement>params['minPerformance']) #& model['converged']$converged 
      ){
        return(x[PredDiff>params['minAlpha'] & sample=='oos'])
      } else {
        return(x[1==0])
      }
      
    } else {
      x[,PredDiff:=0]
      return(x[1==0])
    }
  })
  x=rbindlist(x)
  # print(x)
  return(x)
}
