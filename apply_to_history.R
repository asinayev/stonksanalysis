source("prep_data.R")
source("trade_strategy.R")

read_w_name = function(stockname){
  dt = fread(stockname)
  dt[,stock:=stockname]
  return(dt)
}

pct_diff = function(x,y){(x-y)/x}


stocknames = list.files("data/stocks", pattern="*.csv", full.names=TRUE)
chosenstocks = sample(stocknames,500)
chosenstockdat = rbindlist(lapply(chosenstocks, read_w_name))
chosenstockdat = prep_data(chosenstockdat, oos_start = "2019-01-08", oos_end = "2019-01-16")

results = strategy(params=c("minPerformance"=.02, "minAlpha"=.002), chosenstockdat, chosenstocks)
results[,.(sum(CloseDiff),.N)]

#Visualize parameters
params = expand.grid("minPerformance"=seq(0,.01,.001),"minAlpha"=seq(0,.015,.001))
params$value = apply(params, 1, 
                     function(x) {
                       strategy(params=x, allstocks, chosenstocks)
                       }
                     )
plot(x=params$minAlpha, y=params$minPerformance, cex=params$value*-5 )

