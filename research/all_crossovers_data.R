setwd("stonksanalysis")
source("prep_data.R")
source("store_data.R")
source("crossover_strategy.R")

buySellResearch = function(dates,los, his, closes, shortMA, crosslong, atr, rsi, stdev, valid, buysell_pars, n){
  
  crossovers=data.table(i_bought=c(0), date_bought = dates[300], buy_price=0, sell_price=0, date_sold=dates[300], rsi=0, 
                        rel_atr=0, rel_std=0, rel_price=0, maxDip=0, daysCrossed=0, periodMaxPrice=0)[is.na(sell_price)]
  
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
          crossovers[is.na(sell_price)]$date_sold=dates[i]
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
                           data.table(i_bought=i, date_bought = dates[i], buy_price=closes[i], sell_price=NA, date_sold=dates[i],
                                      rsi=rsi[i], rel_atr=atr[i]/shortMA[i], rel_std=stdev[i]/shortMA[i], 
                                      rel_price = closes[i]/shortMA[i],
                                      maxDip, daysCrossed, periodMaxPrice=closes[i]) ) 
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
          ), c('sell_price', 'date_sold'):=list(closes[i], dates[i])]
      }
    }
  }
  return(crossovers)
}


crossover_research=function(pars=list(), dat){
  print(pars$short_range)
  required_pars = c("short_range",      "long_range",               'crossover_units',
                    "buy_trigger",      
                    "sell_hi",          "sell_lo",                  "sell_atr",         
                    "sell_days",        "sell_last")
  (required_pars %in% names(pars)) %>% all %>% stopifnot
  
  dat = crossover_prep(dat, short_range = pars$short_range,long_range = pars$long_range, crossover_units = pars$crossover_units)
  out = dat[, buySellResearch(Date, loFilled, hiFilled, AdjCloseFilled, short_range_mean, CrossoverLong, atr, rsi, stdev, valid, pars, .N),
      .(stock)] 
  out$short_range = pars$short_range
  out$long_range = pars$long_range
  out$buy_trigger = pars$buy_trigger
  return(out)
}

fulldat = get_dt(name = 'fulldat')


parameterset = expand.grid(short_range=c(28), long_range=c(250), crossover_units=c(0),
                           buy_trigger=seq(-.15,0,.005),
                           sell_hi=c(.15), sell_lo=c(.25), sell_atr = c(100),
                           sell_days=c(180), sell_last=c(T)
)

all_crossovers = parameterset %>% 
  apply(1, as.list) %>%
  parallel::mclapply(crossover_research, dat=fulldat, mc.cores=6) %>%
  rbindlist

all_crossovers[,profit:=pct_diff(sell_price, buy_price, buy_price)]#+(.07*(365-date_sold+date_bought))-(.07*(date_sold-date_bought)/365)]

outsxgb = lapply(2005:2019,function(yr){
  testrange = c(yr,yr+1)
  predictors_train = all_crossovers[profit<.5 & !(year(date_bought) %between% testrange), .(rsi,rel_atr,rel_std,rel_price,maxDip,daysCrossed,buy_trigger)]
  output_train = all_crossovers[profit<.5  & !(year(date_bought) %between% testrange), .(profit)]
  myxgb = xgboost(data = predictors_train %>% as.matrix,
                  label = output_train %>% unlist,
                  params = list(booster='gblinear'),
                  nrounds = 2000, eta=.06, max_depth=1, gamma=20, objective = "reg:squarederror"
                  # ,xgb_model = myxgb
  )
  # myxgb = lm(profit~rsi+rel_atr+rel_std+rel_price+maxDip*daysCrossed+buy_trigger,all_crossovers[profit<.5 & !(year(date_bought) %between% testrange)])
  predictors_test = all_crossovers[profit<.5 & 
                                     (year(date_bought) == yr) & 
                                     buy_trigger %between% c(-.07,-.03), 
                                   .(rsi,rel_atr,rel_std,rel_price,maxDip,daysCrossed,buy_trigger)]
  output_test = all_crossovers[profit<.5  & 
                                 (year(date_bought) == yr) & 
                                 buy_trigger %between% c(-.07,-.03), .(profit)]
  
  preds_table = data.table(output=output_test, predictions = predict(myxgb, predictors_test%>%as.matrix
                                                                     ))
  
  data.table(yr = yr, 
             correlation = cor(preds_table$output.profit, preds_table$predictions),
             return = preds_table[predictions>.04,mean(output.profit)], 
             return_modelfree = preds_table[,mean(output.profit)],
             purchases = preds_table[predictions>.04,.N])
})%>%rbindlist
outsxgb[,mean(return-return_modelfree,na.rm=T)]
outsxgb[,mean(return,na.rm=T)]

# linear booster 2000 rounds eta=.03 returns xvalidated gains ~.035 (almost doubling gains without model)

# fit <- rpart(profit~., data = cbind(predictors_train,output_train), method = 'anova',cp=.003)
# rpart.plot(fit)

predictors_test = all_crossovers[profit<.5 & date_bought %between% testrange & buy_trigger %between% c(-.1,0), .(rsi,rel_atr,rel_std,rel_price,maxDip,daysCrossed,buy_trigger)]
output_test = all_crossovers[profit<.5  & date_bought %between% testrange & buy_trigger %between% c(-.1,0), .(profit)]

preds_table = data.table(output=output_test, predictions = predict(myxgb, predictors_test%>%as.matrix))
cor(preds_table)
preds_table[predictions>.05,.(mean(output.profit),.N)]
preds_table[,.(mean(output.profit),.N)]
preds_table[,.(.N,mean(output.profit)),round(predictions,2)] %>% with(plot(x=round, y=V2, cex=sqrt(N/10))) 
preds_table[,.(.N,mean(output.profit)),round(predictions,2)][order(round)]

abline(a = 0, b=1)
abline(h = 0)

all_crossovers[, mean(profit), .(year(date_bought), buy_trigger)][order(year,buy_trigger)]
