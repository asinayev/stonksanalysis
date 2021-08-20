source('crossover_strategy.R')

buySellResearch = function(los, his, closes, crosslong, atr, rsi, valid, buysell_pars, n){
  
  crossovers=data.table(i_bought=c(0), buy_price=0, sell_price=0, rsi=0, 
                        rel_atr=0, maxDip=0, daysCrossed=0, periodMaxPrice=0)[is.na(sell_price)]
  
  daysCrossed = 0
  maxDip = 0
  
  if(any(!is.na(atr))){
    maxValidi = max(which(!is.na(atr)))
    for (i in 2:maxValidi){
      # increment the counters
      if(!is.na(crosslong[i-1]) && crosslong[i-1]<buysell_pars$buy_trigger){
        daysCrossed = daysCrossed + 1
        maxDip = max(maxDip, buysell_pars$buy_trigger-crosslong[i-1])
      } else { 
        daysCrossed=maxDip=0 
      }
      if(i==maxValidi){ # on the last day
        if(buysell_pars$sell_last){
          crossovers[is.na(sell_price)]$sell_price=closes[i]
        }
        next # and then quit
      }
      
      if(is.na(rsi[i]) || is.na(atr[i]) || is.na(crosslong[i-1]) || is.na(crosslong[i]) ){
        next # skip if necessary values are missing
      }
      
      if(crosslong[i-1]<buysell_pars$buy_trigger &&  # and the crossover was lower than cutoff yesterday
         crosslong[i]>buysell_pars$buy_trigger && # but is higher than cutoff today
         valid[i]){ # Buy if in the valid period){  # and the RSI is low enough
        crossovers = rbind(crossovers,
                           data.table(i_bought=i, buy_price=closes[i], sell_price=NA, 
                                      rsi=rsi[i], rel_atr=atr[i]/closes[i], maxDip, daysCrossed, periodMaxPrice=0) ) 
        }
      getPeriodMaxPrice = function(i_bought, current_i, prices_seq){
        i_bought %>% 
          sapply(function(ay){max(prices_seq[ay:current_i])}) %>%
          as.numeric
      }
      if(nrow(crossovers[is.na(sell_price)])>0){
        crossovers[is.na(sell_price), periodMaxPrice:=getPeriodMaxPrice(i_bought, i, closes)]
        crossovers[is.na(sell_price) & (
          (his[i]>(buy_price*(1+buysell_pars$sell_hi))) |
          ((i-i_bought)>buysell_pars$sell_days) |
          (los[i]<pmax(periodMaxPrice*(1-buysell_pars$sell_lo), 
                      periodMaxPrice-buysell_pars$sell_atr*atr[i]) 
           ) 
          ), sell_price:=closes[i]]
      }
    }
  }
  return(crossovers)
}


crossover_research=function(pars=list(), dat){
  pars=as.list(pars)
  required_pars = c("short_range",      "long_range",               'crossover_units',
                    "buy_trigger",      #"min_dip",                  "buy_trigger_days_max",     "buy_trigger_days_min",  
                    "sell_hi",          "sell_lo",                  "sell_atr",         
                    "sell_days",        "sell_last")
  (required_pars %in% names(pars)) %>% all %>% stopifnot
  
  dat = crossover_prep(dat, short_range = pars$short_range,long_range = pars$long_range, crossover_units = pars$crossover_units)
  dat[, buySellResearch(loFilled, hiFilled, AdjCloseFilled, CrossoverLong, atr, rsi, valid, pars, .N),
      .(stock)] 
}

parameterset = expand.grid(short_range=c(28), long_range=c(250), crossover_units=c(''),
                           buy_trigger=c(0),
                           sell_hi=c(.15), sell_lo=c(.25), sell_atr = c(100),
                           sell_days=c(180), sell_last=c(T)
)

all_crossovers = crossover_research(pars = parameterset, fulldat )


predictors = all_crossovers[profit<.5, .(rsi,rel_atr,maxDip,daysCrossed,periodMaxPrice)]
output = all_crossovers[profit<.5, .(profit)]

myxgb = xgboost(data = predictors %>% as.matrix, 
        label = output %>% unlist, 
        nrounds = 12, eta=.2, max_depth=5,, objective = "reg:squarederror")

preds_table = data.table(output=output, predictions = predict(myxgb, predictors%>%as.matrix))
preds_table[,.(.N,mean(output.profit)),round(predictions,2)] %>% with(plot(x=round, y=V2, cex=sqrt(N/10))) 

abline(a = 0, b=1)
                                                                                                                                          