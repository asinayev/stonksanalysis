crossover_prep = function(indat,
                          short_range = 7,
                          long_range = 28
){
  indat=data.table(indat)
  indat[,Crossover:= pct_diff(
          frollmean(AdjClose, short_range, fill=NA, algo="exact", align="right", na.rm=T), 
          frollmean(AdjClose, long_range,  fill=NA, algo="exact", align="right", na.rm=T),
          frollmean(AdjClose, long_range,  fill=NA, algo="exact", align="right", na.rm=T)),
        stock]
  indat
}


crossover_strategy = function(indat, 
                               train_start, train_end, test_start, test_end, trigger_pct_diff=.05){
  indat=data.table(indat)
  indat[,sample:=ifelse(Date <= train_end & Date>train_start,
                            "train",
                            ifelse(Date <= test_end & Date>test_start,
                                   "test", "none"))]
  
  indat[,AdjCloseFilled:=AdjClose[1], .(cumsum(!is.na(AdjClose)),stock,sample)]
  indat[,Buy:=((shift(Crossover, n=1L, fill=NA, type='lag')<trigger_pct_diff) & (Crossover>trigger_pct_diff))/AdjCloseFilled,
        .(stock,sample)] #Buy when short window is larger than long window today, but not yesterday
  indat[indat[,.I[Date==Date[1]],.(stock,sample)]$V1, Buy:=(Crossover>0)/AdjCloseFilled] #Buy on the first day if short window is larger
  indat[,Buy:=fillna(Buy,0)]
  indat[,HasBought := Buy[1], .(cumsum(Buy != 0),stock,sample)]
  indat[,Sell:=((shift(Crossover, n=1L, fill=NA, type='lag')>-trigger_pct_diff) & (Crossover< -trigger_pct_diff))*HasBought,
        .(stock,sample)] #Sell when short window was larger than long window yesterday, but not today
  indat[,Sell:=fillna(Sell,FALSE)]
  indat[,Own:=cumsum(Buy-Sell),.(stock,sample)]
  indat[indat[,.I[Date==Date[.N]],.(stock,sample)]$V1, Sell:=Own] # Sell on the last day if you own
  indat
}

calcReturnsInPeriod=function(strat, period_label, transaction_fee=.01){
  strat[(Sell-Buy)!=0 & sample==period_label,
       .(profit_strat=
           sum(fillna((Sell-Buy)*AdjCloseFilled,0)) - 
           pct_diff(AdjCloseFilled[.N],AdjCloseFilled[1],of=AdjCloseFilled[1]) - 
           .N*transaction_fee,
         absolute_profit = 
           sum(fillna((Sell-Buy)*AdjCloseFilled,0))-
           .N*transaction_fee,
         mean_volume = mean(volume,na.rm=T),
         trades = .N),
       .(stock)]
}

calcReturns=function(strat, transaction_fee=.01, profit_cutoff=1, volume_cutoff=10000){
  training_returns = calcReturnsInPeriod(strat, 
                                         'train', 
                                         transaction_fee=transaction_fee)
  stocks_to_trade = training_returns[profit_strat>profit_cutoff & 
                                       mean_volume>volume_cutoff, 
                                     .(stock)]
  returns = calcReturnsInPeriod(strat[stocks_to_trade, on='stock'], 
                      'test', 
                      transaction_fee=transaction_fee)
  returns[,.(absolute_profit = mean(absolute_profit),
             stocks=.N,
             trades = sum(trades) )]
}

