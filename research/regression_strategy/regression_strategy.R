regression_prep = function(indat,
                           target_range = 7
){
  indat[,target :=pct_diff(shift(AdjClose, n=target_range, fill=NA, type='lead'), AdjClose), stock]
  indat[,CloseDiff:=pct_diff(AdjClose,shift(AdjClose, n=1L, fill=NA, type='lag')), stock]
  indat[,CloseDiffLag1:=pct_diff(
    shift(AdjClose, n=1L, fill=NA, type='lag'),
    shift(AdjClose, n=2L, fill=NA, type='lag')),
    stock]
  indat[,CloseDiffLag2:=pct_diff(
    shift(AdjClose, n=2L, fill=NA, type='lag'),
    shift(AdjClose, n=3L, fill=NA, type='lag')),
    stock]
  indat[,CloseDiffLag3:=pct_diff(
    shift(AdjClose, n=3L, fill=NA, type='lag'),
    shift(AdjClose, n=4L, fill=NA, type='lag')),
    stock]
  indat[,CloseDiffLag7:=pct_diff(
    AdjClose,
    shift(AdjClose, n=7L, fill=NA, type='lag')),
    stock]
  indat[,CloseDiffLag14:=pct_diff(
    AdjClose,
    shift(AdjClose, n=14L, fill=NA, type='lag')),
    stock]
  indat[,CloseDiff7D:=
          frollmean(CloseDiffLag1, 7, fill=NA, algo="exact", align="right", na.rm=T),
        stock]
  indat[,CloseDiff:=fillna(CloseDiff, CloseDiffLag1)]
  indat[,CloseDiff:=fillna(CloseDiff, CloseDiffLag2)]
  indat[,CloseDiff:=fillna(CloseDiff, CloseDiffLag3)]
  indat[,CloseDiffLag1:=shift(CloseDiff, n=1L, fill=NA, type='lag')]
  indat[,CloseDiffLag2:=shift(CloseDiff, n=2L, fill=NA, type='lag')]
  indat[,CloseDiffLag3:=shift(CloseDiff, n=3L, fill=NA, type='lag')]
  indat
}

get_stock_model = function(stockdat,summary=F){
  model = lm(target~CloseDiff+CloseDiffLag1+#CloseDiffLag2+CloseDiffLag3+
               CloseDiffLag7+CloseDiffLag14+CloseDiff7D,
             data=stockdat, subset=sample=="train") 
  if(summary){
    return(summary(model))
  } else {
    return(model)
  }
}


regression_strategy = function(allstocks=allstocks, 
                    train_start, train_end, test_start, test_end, oos_start, oos_end){
  allstocks[,sample:=ifelse(Date <= train_end & Date>train_start,
                            "train",
                            ifelse(Date <= test_end & Date>test_start,
                                   "test",
                                   ifelse(Date <= oos_end & Date>oos_start, "oos", "none")))]
  stocknames=unique(allstocks$stock)
  x = lapply(stocknames,function(stockname){
    x=allstocks[stock==stockname]
    unique_training_samples=length(unique(x[sample=='train']$AdjClose))
    if(unique_training_samples>30){
      model = get_stock_model(x)
      x[,PredDiff:=predict(model,x)]
    } else {
      x[,PredDiff:=NA]
    }
    error_improvement = pct_diff(x[sample=='test',mean(abs(CloseDiff-mean(CloseDiff,na.rm=T)),na.rm=T)], 
                                 x[sample=='test',mean(abs(PredDiff-CloseDiff),na.rm=T)])
    x[,training_samples:=unique_training_samples]
    x[,error_improvement:=error_improvement]
    return(x[sample=='oos'])
  })
  rbindlist(x)
}

