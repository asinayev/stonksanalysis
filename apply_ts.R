install.packages("data.table")
library(data.table)
library(MASS)


read_w_name = function(stockname){
  dt = fread(stockname)
  dt[,stock:=stockname]
  return(dt)
}

pct_diff = function(x,y){(x-y)/x}

prep_data = function(indat=allstocks,
                     train_start = "2017-01-01",
                     test_start = "2018-01-01",
                     oos_start = "2019-01-01",
                     oos_end = "2020-01-01"){
  setnames(indat, "Adj Close", "AdjClose")
  indat=indat[order(stock,Date)]
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
  indat[,CloseDiff7D:=
              frollmean(CloseDiffLag1, 7, fill=NA, algo="exact", align="right", na.rm=FALSE),
            stock]
  indat[,sample:=ifelse(Date<test_start & Date>train_start,
                            "train",
                            ifelse(Date<oos_start & Date>test_start,
                                   "test",
                                   ifelse(Date<oos_end & Date>oos_start, "oos", "none")))]
}

strategy = function(params, allstocks=allstocks, stocknames=stocknames){
  # print(params)
  x = lapply(stocknames,function(stockname){
    x=allstocks[stock==stockname]
    if(length(unique(x[sample=='train']$AdjClose))>100){
      model = lm(CloseDiff~CloseDiffLag1*CloseDiff7D+CloseDiffLag2+CloseDiffLag3,
                 data=x, subset=sample=="train")
      x[,PredDiff:=predict(model,x)]
      error_improvement = pct_diff(x[sample=='test',mean(abs(CloseDiff-mean(CloseDiff)))], x[sample=='test',mean(abs(PredDiff-CloseDiff))])
      if(!is.na(error_improvement) & (error_improvement>params['minPerformance']) #& model['converged']$converged 
         ){
        return(x[PredDiff>params['minAlpha'] & sample=='test', sum(CloseDiff)])
      } else {
        return(0)
      }
      
    } else {
      return(0)
    }
  })
  names(x)=stocknames
  # print(x)
  return(-sum(unlist(x)))
}

stocknames = list.files("data/stocks", pattern="*.csv", full.names=TRUE)
chosenstocks = sample(stocknames,50)
allstocks = rbindlist(lapply(chosenstocks, read_w_name))
allstocks = prep_data(allstocks)

strategy(params=c("minPerformance"=.01, "minAlpha"=.01), allstocks, chosenstocks)
params = expand.grid("minPerformance"=seq(0,.01,.001),"minAlpha"=seq(0,.011,.001))
params$value = apply(params, 1, 
                     function(x) {
                       strategy(params=x, allstocks, chosenstocks)
                       }
                     )
plot(x=params$minAlpha, y=params$minPerformance, cex=params$value*-5 )

