install.packages("data.table")
library(data.table)
library(MASS)

stocknames = list.files("data/stocks", pattern="*.csv", full.names=TRUE)

read_w_name = function(stockname){
  dt = fread(stockname)
  dt[,stock:=stockname]
  return(dt)
}
pct_diff = function(x,y){(x-y)/x}

allstocks = rbindlist(lapply(stocknames[1:200], read_w_name))
setnames(allstocks, "Adj Close", "AdjClose")
allstocks=allstocks[order(stock,Date)]
allstocks[,CloseDiff:=pct_diff(AdjClose,shift(AdjClose, n=1L, fill=NA, type='lag')), stock]
allstocks[,CloseDiffLag1:=pct_diff(
                                    shift(AdjClose, n=1L, fill=NA, type='lag'),
                                    shift(AdjClose, n=2L, fill=NA, type='lag')),
          stock]
allstocks[,CloseDiffLag2:=pct_diff(
                                    shift(AdjClose, n=2L, fill=NA, type='lag'),
                                    shift(AdjClose, n=3L, fill=NA, type='lag')),
          stock]
allstocks[,CloseDiffLag3:=pct_diff(
                                    shift(AdjClose, n=3L, fill=NA, type='lag'),
                                    shift(AdjClose, n=4L, fill=NA, type='lag')),
          stock]
allstocks[,CloseDiff7D:=
            frollmean(CloseDiffLag1, 7, fill=NA, algo="exact", align="right", na.rm=FALSE),
          stock]


train_start = "2017-01-01"
test_start = "2018-01-01"
oos_start = "2019-01-01"
oos_end = "2020-01-01"
trainstonks = allstocks[Date<test_start & Date>train_start]

strategy = function(params, trainstonks=trainstonks, teststonks=teststonks){
  if(length(unique(trainstonks$AdjClose))>100){
    dlply(trainstonks,)
    model = lm(CloseDiff~CloseDiffLag1*CloseDiff7D+CloseDiffLag2+CloseDiffLag3,trainstonks)
    
    teststonks = stonkdata[Date<oos_start & Date>test_start]
    teststonks[,'predDiff':=predict(model,teststonks)]
    
    error_improvement = pct_diff(mean(abs(mean(teststonks$CloseDiff)-teststonks$CloseDiff)), mean(abs(teststonks$predDiff-teststonks$CloseDiff)))
    if(!is.na(error_improvement) & (error_improvement>.01) & model['converged']$converged ){
      oosstonks = stonkdata[Date<oos_end & Date>oos_start]
      oosstonks[,'predDiff':=predict(model,oosstonks)]
      return(oosstonks[predDiff>.005, sum(CloseDiff)])
    } else {
      return(0)
    }
  }
}
x=lapply(stocknames[100:200], applicator)
