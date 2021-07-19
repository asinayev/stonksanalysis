crossover_prep = function(indat,
                          short_range = 7,
                          mid_range = 28,
                          long_range = 112
){
  indat=data.table(indat)
  indat[,AdjCloseFilled:=AdjClose[1], .(cumsum(!is.na(AdjClose)),stock)]
  indat[,mid_range_mean:=frollmean(AdjClose, mid_range,  fill=NA, algo="exact", align="right", na.rm=T), stock]
  indat[,long_range_mean:=frollmean(AdjClose, long_range,  fill=NA, algo="exact", align="right", na.rm=T), stock]
  indat[,Crossover:= pct_diff(
          frollmean(AdjClose, short_range, fill=NA, algo="exact", align="right", na.rm=T), 
          mid_range_mean,
          mid_range_mean),
        stock]
  indat[,CrossoverLong:= pct_diff( mid_range_mean, long_range_mean, long_range_mean),
        stock]
  indat[frollsum(is.na(AdjCloseFilled),long_range)>0,c("Crossover", "CrossoverLong"):=NA,stock]
  indat
}


crossover_strategy = function(indat, 
                               train_start, train_end, test_start, test_end, 
                              buy_trigger=.05, sell_trigger= -1*buy_trigger,
                              deathcross = F){
  indat=data.table(indat)
  indat[,sample:=ifelse(Date <= train_end & Date>train_start,
                            "train",
                            ifelse(Date <= test_end & Date>test_start,
                                   "test", "none"))]
  indat = indat[sample!='none']
  indat[,Buy:=(shift(Crossover, n=1L, fill=NA, type='lag')<buy_trigger) & 
                 (Crossover>buy_trigger),
        .(stock,sample)] #Buy when short window is larger than long window today, but not yesterday
  if(deathcross){
    indat[,Buy:=(Buy&(CrossoverLong>0))*1]
  }
  # indat[indat[,.I[Date==Date[1]],.(stock,sample)]$V1, 
  #       Buy:=(Crossover>buy_trigger)] #Buy on the first day if short window is larger
  indat[,Buy:=fillna(Buy,0)]
  indat[,Sell:=((shift(Crossover, n=1L, fill=NA, type='lag')> sell_trigger) & 
                  (Crossover< sell_trigger)),
        .(stock,sample)] #Sell when short window was larger than long window yesterday, but not today
  indat[,Sell:=fillna(Sell,0)]
  indat[,buy_period:=cumsum(Buy!=0),.(stock,sample)]
  indat[,sell_period:=cumsum(Sell!=0),.(stock,sample)]
  indat[!is.na(AdjCloseFilled),
        Buy:=ifelse(cumsum(Buy)>1,0,Buy/AdjCloseFilled),
        .(stock,sample,sell_period)] #do not buy again until you sell
  indat[,Sell:=ifelse(cumsum(Sell)>1,0,Sell),.(stock,sample,buy_period)] #do not sell again until you buy
  indat[,buy_period:=cumsum(Buy!=0),.(stock,sample)] #needs to be set again because some buys were deleted
  indat[,LastBought := Buy[1], .(stock,sample,buy_period)]
  indat[,Sell := fillna(Sell*LastBought,0)]
  indat[,Own:=cumsum(Buy-Sell),.(stock,sample)]
  indat[indat[,.I[Date==Date[.N]],.(stock,sample)]$V1, Sell:=Own] # Sell on the last day if you own
  indat
}

stockAvgReturns=function(strat, period_label, transaction_fee=.01){
    strat[(Sell-Buy)!=0 & sample==period_label,
         .(rel_profit=
             (abs(cumprod((Sell-Buy)*AdjCloseFilled)[.N])-1) - 
             pct_diff(AdjCloseFilled[.N],AdjCloseFilled[1],of=AdjCloseFilled[1]) - 
             .N*transaction_fee,
           absolute_profit = 
             (abs(cumprod((Sell-Buy)*AdjCloseFilled)[.N])-1)-
             .N*transaction_fee,
           median_volume = median(volume,na.rm=T),
           trades = .N),
         .(stock)]
}

calcReturns=function(strat, transaction_fee=.01, profit_cutoff=1, volume_cutoff=10000, summary=T){
  training_returns = stockAvgReturns(strat, 
                                         'train', 
                                         transaction_fee=transaction_fee)
  stocks_to_trade = training_returns[rel_profit>profit_cutoff & 
                                       median_volume>volume_cutoff, 
                                     .(stock)]
  returns = calcReturnsInPeriod(strat[stocks_to_trade, on='stock'], 
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

crossoverReturns=function(pars=list('short_range'=3, 'mid_range'=28, 'buy_trigger'=.05, 'sell_trigger'=-.05, 'deathcross'=F, 'profit'=-10), 
                          dat, date, end_date=date+365, start_date=date-365, summary_only=T, transaction_fee=.01, stock_avg=T){
  pars=as.list(pars)
  dat %>%
    crossover_prep(pars$short_range,pars$mid_range,long_range = 154) %>%
    crossover_strategy(train_start = start_date,
                       train_end = date, date, end_date, 
                       buy_trigger = pars$buy_trigger, sell_trigger = pars$sell_trigger,
                       deathcross=pars$deathcross) %>%
    calcReturns(transaction_fee=transaction_fee, profit_cutoff=pars$profit, volume_cutoff=100000, summary=summary_only)
}