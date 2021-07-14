library(data.table)

fillna = function(x, fill){
  ifelse(is.na(x),fill,x)
}
pct_diff = function(x,y,of=x){(x-y)/of}

basic_prep = function(indat,
                      rename_from = "AdjClose",
                      rename_to = "AdjClose",
                      end_date = 0){
  setnames(indat, rename_from, rename_to, skip_absent=TRUE)
  indat$Date=as.Date(indat$Date)
  all_combinations = expand.grid(
    Date = seq(min(indat$Date, na.rm = T),
               max(indat$Date, na.rm = T),1 ),
    stock = unique(indat$stock)
  )
  indat=data.table(indat)[all_combinations, on = c('Date','stock')]
  if (end_date!=0){
    indat=indat[Date<=end_date]
  }
  indat[order(stock,Date)]
}
