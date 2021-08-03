crossover_prep = function(indat,
                          short_range = 7,
                          mid_range = 28,
                          long_range = 112
){
  indat=data.table(indat)
  indat[,short_range_mean:=frollmean(AdjClose, short_range, fill=NA, algo="exact", align="right", na.rm=T), stock]
  indat[,mid_range_mean:=frollmean(AdjClose, mid_range,  fill=NA, algo="exact", align="right", na.rm=T), stock]
  indat[,long_range_mean:=frollmean(AdjClose, long_range,  fill=NA, algo="exact", align="right", na.rm=T), stock]
  indat[,Crossover:= pct_diff(short_range_mean, 
                              mid_range_mean,
                              mid_range_mean),
        stock]
  indat[,CrossoverLong:= pct_diff( short_range_mean, long_range_mean, long_range_mean),
        stock]
  indat[frollsum(is.na(AdjCloseFilled),long_range)>0,c("Crossover", "CrossoverLong"):=NA,stock]
  indat
}

buySellSeq = function(los, his, closes, crosslong, atr, buysell_pars, n){
  
  shares_sold = rep(0,n) #or negative when bought
  crosslong_lagged = shift(crosslong, n=1L, fill=NA, type='lag')
  
  lastBoughtPrice = -1
  periodMax = -1
  daysSinceLoss = buysell_pars$cooloff
  daysSincePurchase = buysell_pars$sell_days
  for (i in 1:n){
    daysSinceLoss = daysSinceLoss + 1
    daysSincePurchase = daysSincePurchase + 1
    if(i==n){
      if(lastBoughtPrice>0 && buysell_pars$sell_last){
        shares_sold[i]= 1/lastBoughtPrice  
      }
      next
    }
    if(is.na(closes[i]) || is.na(crosslong_lagged[i]) || is.na(crosslong[i]) || is.na(atr[i])){
      next
    }
    if(lastBoughtPrice==-1 && # Buy if not already holding
       crosslong_lagged[i]<buysell_pars$buy_trigger &&  # and the crossover was lower than cutoff yesterday
       crosslong[i]>buysell_pars$buy_trigger && # but is higher than cutoff today
       daysSinceLoss>buysell_pars$cooloff){ # and enough days have passed since the last loss
      lastBoughtPrice = periodMax = closes[i]
      shares_sold[i]= -1/lastBoughtPrice
      daysSincePurchase=0
    } else if (lastBoughtPrice>0){ # When you own
      periodMax = max(periodMax, closes[i])
      if(his[i]>lastBoughtPrice*(1+buysell_pars$sell_hi)){ #And the high is high enough
        shares_sold[i]= 1/lastBoughtPrice
        lastBoughtPrice = periodMax = -1
      } else if (los[i]<max(periodMax*(1-buysell_pars$sell_lo), 
                            periodMax-buysell_pars$sell_atr*atr[i]) ){ #Or low is low enough
        shares_sold[i]= 1/lastBoughtPrice
        lastBoughtPrice = periodMax = -1
        daysSinceLoss=0
      } else if (daysSincePurchase>buysell_pars$sell_days){ # Or you held long enough
        shares_sold[i]= 1/lastBoughtPrice
        lastBoughtPrice = periodMax = -1
      }
    }
  }
  
  return(shares_sold)
}

crossover_strategy = function(indat, 
                              train_start, train_end, test_start, test_end, 
                              buysell_pars){
  indat=data.table(indat)
  indat[,sample:=ifelse(Date <= train_end & Date>train_start,
                        "train",
                        ifelse(Date <= test_end & Date>test_start,
                               "test", "none"))]
  indat = indat[sample!='none']
  indat[,BuySell:= buySellSeq(loFilled, hiFilled, AdjCloseFilled, CrossoverLong, atr, buysell_pars, .N),
        .(stock,sample)] #Buy when short window is larger than long window today, but not yesterday
  indat[,Own:=cumsum(-1*BuySell),.(stock,sample)]
  indat[,LastBought:=AdjCloseFilled[1],.(Own, stock, sample)]
  indat
}

stockAvgReturns=function(strat, period_label, transaction_fee=.01, profit_cutoff){
  strat[BuySell!=0 & sample==period_label,
        .(rel_profit=
            (abs(cumprod(BuySell*AdjCloseFilled)[.N])-1) - 
            pct_diff(AdjCloseFilled[.N],AdjCloseFilled[1],of=AdjCloseFilled[1]) - 
            .N*transaction_fee,
          absolute_profit = 
            min((abs(cumprod( pmin(1,BuySell*AdjCloseFilled) )[.N])-1),profit_cutoff)-
            .N*transaction_fee,
          median_volume = median(volume,na.rm=T),
          days_held = cumsum(BuySell/abs(BuySell)*as.integer(Date))[.N],
          trades = .N),
        .(stock)]
}

calcReturns=function(strat, transaction_fee=.01, profit_cutoff=1, volume_cutoff=10000, summary=T){
  # if(pick_stocks){
  #   training_returns = stockAvgReturns(strat, 
  #                                      'train', 
  #                                      transaction_fee=transaction_fee)
  #   stocks_to_trade = training_returns[rel_profit>profit_cutoff & 
  #                                        median_volume>volume_cutoff, 
  #                                      .(stock)]
  # } else {
    stocks_to_trade = data.table(stock = unique(strat$stock))
  # }
  returns = stockAvgReturns(strat,#[stocks_to_trade, on='stock'], 
                            'test', 
                            transaction_fee=transaction_fee,
                            profit_cutoff=profit_cutoff+.1)
  if(summary & nrow(returns)>0){
    return(returns[,.(avg_profit = mean(absolute_profit),
                      median_profit = median(absolute_profit),
                      median_margin = median(rel_profit),
                      avg_days_held=mean(days_held), 
                      stocks=.N,
                      DaysHeldPerPurchase = sum(days_held)/sum(trades/2),
                      trades = sum(trades) )])
  } else if (summary) {
    return(returns[,.(avg_profit = 0,
                      median_profit = 0,
                      median_margin = -.08,
                      avg_days_held=0, 
                      stocks=0,
                      DaysHeldPerPurchase = 0,
                      trades = 0 )])
  } else {
    # return(strat[stocks_to_trade, on='stock'])
    return(strat)
  }
}

crossoverReturns=function(pars=list(), 
                          dat, date, end_date=date+365, start_date=date-365, summary_only=T, transaction_fee=.01, stock_avg=T){
  pars=as.list(pars)
  required_pars = c('buy_trigger', 'cooloff', 
                    'sell_days', 'sell_lo', 'sell_hi', 'sell_atr', 'sell_last')
  (required_pars %in% names(pars)) %>% all %>% stopifnot
  
  dat %>%
    crossover_prep(pars$short_range,pars$mid_range,long_range = pars$long_range) %>%
    crossover_strategy(train_start = start_date,
                       train_end = date, date, end_date, 
                       buysell_pars = pars) %>%
    calcReturns(transaction_fee=transaction_fee, profit_cutoff=pars$sell_hi, volume_cutoff=10000, summary=summary_only)
}
