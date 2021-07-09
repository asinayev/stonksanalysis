library(data.table)

pct_diff = function(x,y){(x-y)/x}
prep_data = function(indat=allstocks,
                     train_start = "2017-01-01",
                     test_start = "2018-01-01",
                     oos_start = "2019-01-01", 
                     oos_end = "2020-01-01", #inclusive
                     rename_from = "Adj Close",
                     rename_to = "AdjClose",
                     train_end = test_start, #inclusive
                     test_end = oos_start, #inclusive
                     future=F){
  indat=data.table(indat)
  setnames(indat, rename_from, rename_to)
  if(future){
    future_row = indat[, .SD[1], stock]
    future_row[,Date:=as.Date(oos_end)]
    future_row[,AdjClose:=NA]
    indat=rbindlist(list(indat,future_row))
  }
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
  indat[,sample:=ifelse(Date <= train_end & Date>train_start,
                        "train",
                        ifelse(Date <= test_end & Date>test_start,
                               "test",
                               ifelse(Date <= oos_end & Date>oos_start, "oos", "none")))]
  indat
}
