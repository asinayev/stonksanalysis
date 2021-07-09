source("prep_data.R", local=T)
source("trade_strategy.R", local=T)

install.packages("readxl")
library(tidyquant)
library(TTR)

read_w_name = function(stockname){
  if(exists(stockname)){
    dat = Ad(get(stockname))
    names(dat)='AdjClose'
    return(
      data.frame(
        "Date"=index(dat),
        "AdjClose"=dat,
        "stock"=stockname
      )
    )
  }
}



chosenstocks = TTR::stockSymbols()$Symbol
getSymbols(chosenstocks, from = "2019-02-01", to = "2021-08-01", auto.assign = TRUE)

chosenstockdat = rbindlist(lapply(chosenstocks, read_w_name))
chosenstockdat = prep_data(chosenstockdat, 
                           train_start = "2019-02-01",
                           train_end = "2020-02-01",
                           test_start = "2020-04-15",
                           test_end = "2021-04-15",
                           oos_start = "2021-04-15",
                           oos_end = "2021-08-15",
                           rename_from="AdjClose",rename_to="AdjClose")

results = strategy(params=c("minPerformance"=.02, "minAlpha"=.002), chosenstockdat, chosenstocks)
results[,.(sum(CloseDiff),.N),stock]
