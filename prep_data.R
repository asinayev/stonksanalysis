library(data.table)

pct_diff = function(x,y){(x-y)/x}
prep_data = function(indat=allstocks,
                     rename_from = "Adj Close",
                     rename_to = "AdjClose",
                     future_date=F){
  indat=data.table(indat)
  setnames(indat, rename_from, rename_to)
  if(future_date!=F){
    future_row = indat[, .SD[1], stock]
    future_row[,Date:=as.Date(future_date)]
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
  indat
}
