crossover_prep = function(indat,
                          short_range = 7,
                          mid_range = 28,
                          long_range = 112
){
  indat=data.table(indat)
  indat[,AdjCloseFilled:=AdjClose[1], .(cumsum(!is.na(AdjClose)),stock)]
  indat[,hiFilled:=high[1], .(cumsum(!is.na(high)),stock)]
  indat[,loFilled:=low[1], .(cumsum(!is.na(low)),stock)]
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

buySellSeq = function(los, his, closes, crosslong, buy_trigger, sell_trigger, cooloff, sell_after, sell_last, n){
  out = rep(0,n)
  lastBoughtPrice = -1
  days_since_loss = cooloff
  days_since_purchase = sell_after
  crosslong_lagged = shift(crosslong, n=1L, fill=NA, type='lag')
  for (i in 1:n){
    days_since_loss = days_since_loss + 1
    days_since_purchase = days_since_purchase + 1
    if(is.na(closes[i]) || is.na(crosslong_lagged[i]) || is.na(crosslong[i])){
      next
    }
    if(lastBoughtPrice==-1 && # Buy if not already holding
       crosslong_lagged[i]<buy_trigger &&  # and the crossover was lower than cutoff yesterday
       crosslong[i]>buy_trigger && # but is higher than cutoff today
       days_since_loss>cooloff){ 
      lastBoughtPrice = closes[i]
      out[i]= -1/lastBoughtPrice
      days_since_purchase=0
    } else if (lastBoughtPrice>0) # Sell when you own
      if(his[i]>lastBoughtPrice*(1+sell_trigger)){ #And the high is high enough
        out[i]= 1/lastBoughtPrice
        lastBoughtPrice=-1
      } else if (los[i]<lastBoughtPrice*(1-sell_trigger)){ #Or low is low enough
        out[i]= 1/lastBoughtPrice
        lastBoughtPrice=-1
        days_since_loss=0
      } else if (days_since_purchase>sell_after){ # Or you held long enough
        out[i]= 1/lastBoughtPrice
        lastBoughtPrice=-1
        if(closes[i]<lastBoughtPrice){
          days_since_loss=0
        }
      }
    if(i==n && lastBoughtPrice>0 && sell_last){
      out[i]= 1/lastBoughtPrice
    }
  }
  
  return(out)
}

crossover_strategy = function(indat, 
                               train_start, train_end, test_start, test_end, 
                              buy_trigger=.05, sell_trigger= -1*buy_trigger,
                              cooloff=365, sell_after=180,
                              sell_last_day = T){
  indat=data.table(indat)
  indat[,sample:=ifelse(Date <= train_end & Date>train_start,
                            "train",
                            ifelse(Date <= test_end & Date>test_start,
                                   "test", "none"))]
  indat = indat[sample!='none']
  indat[,BuySell:= buySellSeq(loFilled, hiFilled, AdjCloseFilled, CrossoverLong, buy_trigger, sell_trigger, cooloff,sell_after, sell_last_day, .N),
        .(stock,sample)] #Buy when short window is larger than long window today, but not yesterday
  indat[,Own:=cumsum(-1*BuySell),.(stock,sample)]
  indat[,LastBought:=AdjCloseFilled[1],.(Own, stock, sample)]
  indat
}

stockAvgReturns=function(strat, period_label, transaction_fee=.01){
    strat[BuySell!=0 & sample==period_label,
         .(rel_profit=
             (abs(cumprod(BuySell*AdjCloseFilled)[.N])-1) - 
             pct_diff(AdjCloseFilled[.N],AdjCloseFilled[1],of=AdjCloseFilled[1]) - 
             .N*transaction_fee,
           absolute_profit = 
             (abs(cumprod(BuySell*AdjCloseFilled)[.N])-1)-
             .N*transaction_fee,
           median_volume = median(volume,na.rm=T),
           trades = .N),
         .(stock)]
}

calcReturns=function(strat, transaction_fee=.01, profit_cutoff=1, volume_cutoff=10000, summary=T, pick_stocks = F){
  if(pick_stocks){
    training_returns = stockAvgReturns(strat, 
                                           'train', 
                                           transaction_fee=transaction_fee)
    stocks_to_trade = training_returns[rel_profit>profit_cutoff & 
                                         median_volume>volume_cutoff, 
                                       .(stock)]
  } else {
      stocks_to_trade = data.table(stock = unique(strat$stock))
    }
  returns = stockAvgReturns(strat[stocks_to_trade, on='stock'], 
                      'test', 
                      transaction_fee=transaction_fee)
  if(summary & nrow(returns)>0){
    return(returns[,.(absolute_profit = mean(absolute_profit),
               stocks=.N,
               trades = sum(trades) )])
  } else if (summary) {
    return(returns[,.(absolute_profit = 0,
                      stocks=0,
                      trades =0 )])
  } else {
    return(strat[stocks_to_trade, on='stock'])
  }
}

crossoverReturns=function(pars=list('short_range'=3, 'mid_range'=28, 
                                    'buy_trigger'=.05, 'sell_trigger'=-.05, 
                                    'cooloff'=180,), 
                          dat, date, end_date=date+365, start_date=date-365, summary_only=T, transaction_fee=.01, stock_avg=T){
  pars=as.list(pars)
  dat %>%
    crossover_prep(pars$short_range,pars$mid_range,long_range = pars$long_range) %>%
    crossover_strategy(train_start = start_date,
                       train_end = date, date, end_date, 
                       buy_trigger = pars$buy_trigger, sell_trigger = pars$sell_trigger,
                       cooloff = pars$cooloff, sell_after=pars$sell_after, sell_last_day = pars$sell_last_day) %>%
    calcReturns(transaction_fee=transaction_fee, profit_cutoff=pars$profit, volume_cutoff=10000, summary=summary_only)
}
