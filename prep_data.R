library(data.table)

fillna = function(x, fill){
  ifelse(is.na(x),fill,x)
}
pct_diff = function(x,y){(x-y)/x}
prep_data = function(indat=allstocks,
                     rename_from = "AdjClose",
                     rename_to = "AdjClose",
                     target_range = 7
                     ){

  setnames(indat, rename_from, rename_to, skip_absent=TRUE)
  all_combinations = expand.grid(
    Date = seq(min(indat$Date, na.rm = T),max(indat$Date, na.rm = T),1),
    stock = unique(indat$stock)
  )
  indat=data.table(indat)[all_combinations, on = c('Date','stock')]
  indat=indat[order(stock,Date)]
  
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
