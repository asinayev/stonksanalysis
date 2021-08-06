crossover_prep = function(indat,
                          short_range = 7,
                          long_range = 112
){
  indat=data.table(indat)
  indat[,short_range_mean:=frollmean(AdjClose, short_range, fill=NA, algo="exact", align="right", na.rm=T), stock]
  indat[,long_range_mean:=frollmean(AdjClose, long_range,  fill=NA, algo="exact", align="right", na.rm=T), stock]
  indat[,CrossoverLong:= pct_diff( short_range_mean, long_range_mean, long_range_mean),
        stock]
  indat[frollsum(is.na(AdjCloseFilled),long_range)>0,c("CrossoverLong"):=NA,stock]
  indat
}

buySellSeq = function(los, his, closes, crosslong, atr, valid, buysell_pars, n){
  
  shares_sold = rep(0,n) #or negative when bought
  crosslong_lagged = shift(crosslong, n=1L, fill=NA, type='lag')
  
  lastBoughtPrice = -1
  periodMax = -1
  daysSinceLoss = buysell_pars$cooloff
  daysSincePurchase = buysell_pars$sell_days
  daysCrossed = 0
  for (i in 1:n){
    # increment the counters
    daysSinceLoss = daysSinceLoss + 1
    daysSincePurchase = daysSincePurchase + 1
    if(!is.na(crosslong_lagged[i]) && crosslong_lagged[i]<buysell_pars$buy_trigger){
      daysCrossed = daysCrossed + 1
    } else { 
      daysCrossed=0 
    }
    if(i==n){ # on the last day
      if(lastBoughtPrice>0 && buysell_pars$sell_last){
        shares_sold[i]= 1/lastBoughtPrice   # sell if you own
      }
      next # and then quit
    }
    if(is.na(closes[i]) || is.na(crosslong_lagged[i]) || is.na(crosslong[i]) || is.na(atr[i])){
      next # skip if necessary values are missing
    }
    if(valid[i] && # Buy if in the valid period
       lastBoughtPrice==-1 && # and not already holding
       crosslong_lagged[i]<buysell_pars$buy_trigger &&  # and the crossover was lower than cutoff yesterday
       crosslong[i]>buysell_pars$buy_trigger && # but is higher than cutoff today
       daysSinceLoss>buysell_pars$cooloff && # and enough days have passed since the last loss
       daysCrossed<buysell_pars$buy_trigger_days){ # and the lines have been crossed long enough
      lastBoughtPrice = periodMax = closes[i]
      shares_sold[i]= -1/lastBoughtPrice
      daysSincePurchase=0
    } else if (lastBoughtPrice>0){ # When you own
      periodMax = max(periodMax, closes[i])
      if(his[i]>lastBoughtPrice*(1+buysell_pars$sell_hi)){ #And the high is high enough
        shares_sold[i]= 1/lastBoughtPrice
        lastBoughtPrice = periodMax = -1
      } else if (daysSincePurchase>buysell_pars$sell_days){ # Or you held long enough
        shares_sold[i]= 1/lastBoughtPrice
        lastBoughtPrice = periodMax = -1
      } else if (los[i]<max(periodMax*(1-buysell_pars$sell_lo), 
                            periodMax-buysell_pars$sell_atr*atr[i]) ){ #Or low is low enough
        shares_sold[i]= 1/lastBoughtPrice
        lastBoughtPrice = periodMax = -1
        daysSinceLoss=0
      }
    }
  }
  
  return(shares_sold)
}

crossover_strategy = function(indat, 
                              buysell_pars){
  indat=data.table(indat)
  indat[,BuySell:= buySellSeq(loFilled, hiFilled, AdjCloseFilled, CrossoverLong, atr, valid, buysell_pars, .N),
        .(stock)] #Buy when short window is larger than long window today, but not yesterday
  indat[,Own:=cumsum(-1*BuySell),.(stock)]
  indat[,LastBought:=AdjCloseFilled[1],.(Own, stock)]
  indat
}

saleReturns=function(strat, transaction_fee=.01, profit_cutoff){
  transactions = strat[BuySell!=0]
  transactions[, buyperiod:= cumsum(BuySell<0)]
  transactions[, days_held := cumsum(as.integer(Date)*BuySell/abs(BuySell)), buyperiod]
  transactions[BuySell>0,
               .(stock, Date,
                 absolute_profit = ifelse((BuySell*AdjCloseFilled-1)>profit_cutoff,0,BuySell*AdjCloseFilled-1-transaction_fee),
                 days_held)]
}

calcReturns=function(strat, transaction_fee=.01, profit_cutoff=1, summary=T){
  if(summary){
    returns = saleReturns(strat,
                          transaction_fee=transaction_fee,
                          profit_cutoff=profit_cutoff)
    if(nrow(returns)>0){
      return(returns[,.(avg_profit = mean(absolute_profit),
                        median_profit = median(absolute_profit),
                        sd_profit = sd(absolute_profit),
                        days_held_per_purchase = mean(days_held),
                        purchases = .N )])
    } else {
      return(returns[,.(avg_profit = 0,
                        median_profit = 0,
                        sd_profit = 0,
                        days_held_per_purchase = 0,
                        purchases = 0 )])
    } 
  } else {
    return(strat)
  }
}

crossoverReturns=function(pars=list(), 
                          dat, summary_only=T, transaction_fee=.01){
  pars=as.list(pars)
  required_pars = c("short_range",      "long_range",       
                    "buy_trigger",      "cooloff",          "buy_trigger_days", 
                    "sell_hi",          "sell_lo",          "sell_atr",         
                    "sell_days",        "sell_last")
  (required_pars %in% names(pars)) %>% all %>% stopifnot
  
  dat %>%
    crossover_prep(short_range = pars$short_range,long_range = pars$long_range) %>%
    crossover_strategy(buysell_pars = pars) %>%
    calcReturns(transaction_fee=transaction_fee, profit_cutoff=.5, 
                summary=summary_only)
}