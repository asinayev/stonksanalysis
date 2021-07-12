strategy = function(allstocks=allstocks, stocknames=stocknames,
                    train_start, train_end, test_start, test_end, oos_start, oos_end){
  allstocks[,sample:=ifelse(Date <= train_end & Date>train_start,
                            "train",
                            ifelse(Date <= test_end & Date>test_start,
                                   "test",
                                   ifelse(Date <= oos_end & Date>oos_start, "oos", "none")))]
  x = lapply(stocknames,function(stockname){
    x=allstocks[stock==stockname]
    unique_training_samples=length(unique(x[sample=='train']$AdjClose))
    x[,training_samples:=unique_training_samples]
    if(unique_training_samples>30){
      model = lm(CloseDiff~CloseDiffLag1*CloseDiff7D+CloseDiffLag2+CloseDiffLag3,
                 data=x, subset=sample=="train")
      x[,PredDiff:=predict(model,x)]
    } else {
      x[,PredDiff:=NA]
    }
    error_improvement = pct_diff(x[sample=='test',mean(abs(CloseDiff-mean(CloseDiff)))], x[sample=='test',mean(abs(PredDiff-CloseDiff))])
    x[,error_improvement:=error_improvement]
    return(x[sample=='oos'])
  })
  rbindlist(x)
}
