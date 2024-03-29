crossover_prep = function(indat,
                          short_range = 7,
                          long_range = 112,
                          crossover_units = "long_range_mean"
){
  indat=data.table(indat)
  indat[,short_range_mean:=frollmean(AdjClose, short_range, fill=NA, algo="exact", align="right", na.rm=T), stock]
  indat[,long_range_mean:=frollmean(AdjClose, long_range,  fill=NA, algo="exact", align="right", na.rm=T), stock]
  if(crossover_units=='atr'){
    indat[,CrossoverLong:= pct_diff( short_range_mean, long_range_mean, atr),
          stock]
  } else {
    indat[,CrossoverLong:= pct_diff( short_range_mean, long_range_mean, long_range_mean),
          stock]
  }
  indat[frollsum(is.na(AdjCloseFilled),long_range)>0,c("CrossoverLong"):=NA,stock]
  indat
}

buySellSeq = function(los, his, closes, crosslong, atr, rsi, valid, buysell_pars, n){
  
  shares_sold = rep(0,n) #or negative when bought
  lastBoughtPrice = periodMaxPrice = -1
  daysSincePurchase = buysell_pars$sell_days
  daysCrossed = 0
  maxDip = 0
  
  if(any(!is.na(atr))){
    maxValidi = max(which(!is.na(atr)))
    for (i in 2:maxValidi){
      # increment the counters
      daysSincePurchase = daysSincePurchase + 1
      if(!is.na(crosslong[i-1]) && crosslong[i-1]<buysell_pars$buy_trigger){
        daysCrossed = daysCrossed + 1
        maxDip = max(maxDip, buysell_pars$buy_trigger-crosslong[i-1])
      } else { 
        daysCrossed=maxDip=0 
      }
      if(i==maxValidi){ # on the last day
        if(lastBoughtPrice>0 && buysell_pars$sell_last){
          shares_sold[i]= 1/lastBoughtPrice   # sell if you own
        }
        next # and then quit
      }
      
      if(is.na(rsi[i]) || is.na(atr[i]) || is.na(crosslong[i-1]) || is.na(crosslong[i]) ){
        next # skip if necessary values are missing
      }
      
      if(crosslong[i-1]<buysell_pars$buy_trigger &&  # and the crossover was lower than cutoff yesterday
         crosslong[i]>buysell_pars$buy_trigger && # but is higher than cutoff today
         daysCrossed>buysell_pars$buy_trigger_days_min && # and the lines have been crossed long enough
         daysCrossed<buysell_pars$buy_trigger_days_max && # but not too long
         valid[i] && # Buy if in the valid period
         lastBoughtPrice==-1 && # and not already holding
         maxDip>buysell_pars$min_dip && # and the max dip is big enough
         atr[i]/closes[i]>buysell_pars$buy_atr_min && # and the ATR is high enough
         rsi[i]<buysell_pars$buy_rsi_max){  # and the RSI is low enough
        lastBoughtPrice = periodMaxPrice = closes[i]
        shares_sold[i]= -1/lastBoughtPrice
        daysSincePurchase=0
      } else if (lastBoughtPrice>0){ # When you own
        periodMaxPrice = max(periodMaxPrice, closes[i])
        if(his[i]>lastBoughtPrice*(1+buysell_pars$sell_hi)){ #And the high is high enough
          shares_sold[i]= 1/lastBoughtPrice
          lastBoughtPrice = periodMaxPrice = -1
        } else if (daysSincePurchase>buysell_pars$sell_days){ # Or you held long enough
          shares_sold[i]= 1/lastBoughtPrice
          lastBoughtPrice = periodMaxPrice = -1
        } else if (los[i]<max(periodMaxPrice*(1-buysell_pars$sell_lo), 
                              periodMaxPrice-buysell_pars$sell_atr*atr[i]) ){ #Or low is low enough
          shares_sold[i]= 1/lastBoughtPrice
          lastBoughtPrice = periodMaxPrice = -1
        }
      }
    }
  }
  return(shares_sold)
}

crossover_strategy = function(indat, 
                              buysell_pars){
  indat=data.table(indat)
  indat[,BuySell:= buySellSeq(loFilled, hiFilled, AdjCloseFilled, CrossoverLong, atr, rsi, valid, buysell_pars, .N),
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
                        sd_date = sd(Date),
                        days_held_per_purchase = mean(days_held),
                        purchases = .N )])
    } else {
      return(returns[,.(avg_profit = 0,
                        median_profit = 0,
                        sd_profit = 0,
                        sd_date=0,
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
  required_pars = c("short_range",      "long_range",               'crossover_units',
                    "buy_trigger",      "min_dip",                  "buy_trigger_days_max",     "buy_trigger_days_min",  
                    "sell_hi",          "sell_lo",                  "sell_atr",         
                    "sell_days",        "sell_last")
  (required_pars %in% names(pars)) %>% all %>% stopifnot
  
  dat %>%
    crossover_prep(short_range = pars$short_range,long_range = pars$long_range, crossover_units = pars$crossover_units) %>%
    crossover_strategy(buysell_pars = pars) %>%
    calcReturns(transaction_fee=transaction_fee, profit_cutoff=.5, 
                summary=summary_only)
}