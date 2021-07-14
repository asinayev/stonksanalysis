library(data.table)

fillna = function(x, fill){
  ifelse(is.na(x),fill,x)
}
pct_diff = function(x,y,of=x){(x-y)/of}

basic_prep = function(indat,
                      rename_from = "AdjClose",
                      rename_to = "AdjClose"){
  setnames(indat, rename_from, rename_to, skip_absent=TRUE)
  all_combinations = expand.grid(
    Date = seq(min(as.Date(indat$Date), na.rm = T),
               max(as.Date(indat$Date), na.rm = T),1 ),
    stock = unique(indat$stock)
  )
  indat=data.table(indat)[all_combinations, on = c('Date','stock')]
  indat[order(stock,Date)]
}
