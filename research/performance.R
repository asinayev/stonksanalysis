drawdown = function(x){
  Reduce(function(.x,.y){
    if((.x+.y)>0){0} else {.x+.y}
  },
  x,0, accumulate=T)[-1]
}

performance=function(date, outcome, days_held, symbol, sell_date=date, hold_max=F, buy_per_day_max=5, hold_same_max=F){
  results = data.table(date=date,outcome=outcome,days_held=days_held,symbol=symbol,sell_date=sell_date)
  results=na.omit(results)
  if(hold_max | buy_per_day_max | hold_same_max){
    results[, row_id := .I]
    results[, to_include := FALSE]
    held_positions <- results[T==F,.(symbol,sell_date)]
    for (current_date in unique(results$date)) {
      
      # 1. Update Portfolio: "Sell" anything that was due to be sold before today.
      # We keep positions where sell_date is today or in the future.
      held_positions <- held_positions[sell_date >= current_date]
      
      # 2. Get today's potential trades from the main table
      candidates_today <- results[date == current_date]
      
      # 3. Initialize daily counters
      buys_made_today      <- 0
      symbols_bought_today <- list() # Tracks buys of specific symbols for today
      
      # If there are no potential trades for today, skip to the next day
      if (nrow(candidates_today) == 0) next
      
      # 4. Decide which trades to make today
      for (i in 1:nrow(candidates_today)) {
        candidate_row <- candidates_today[i]
        
        # --- Check all constraints IN ORDER ---
        
        # Constraint A: Do we have capacity in the portfolio?
        # (Current holdings + what we've already decided to buy today)
        if (hold_max & (nrow(held_positions) + buys_made_today) >= hold_max) {
          break # Portfolio is full, can't buy anything else today
        }
        
        # Constraint B: Have we already bought the max number of symbols for today?
        if (buys_made_today & buys_made_today >= buy_per_day_max) {
          break # Daily buy limit reached
        }
        
        # Constraint C: Do we already hold too much of this symbol?
        current_symbol <- candidate_row$symbol
        if (hold_same_max & held_positions[,sum(symbol == current_symbol)] >= hold_same_max) {
          next # Skip to the next candidate row
        }
        
        # --- If all checks pass, we "buy" the symbol ---
        
        # Mark this row in the original table as included
        results[row_id == candidate_row$row_id, to_include := TRUE]
        
        # Update our daily counters
        buys_made_today <- buys_made_today + 1

        # Add the newly acquired position to our portfolio for the next day's calculation
        new_position <- candidate_row[, .(symbol, sell_date)]
        held_positions <- rbindlist(list(held_positions, new_position))
      }
    }
    
    # Clean up by removing the temporary row_id column
    results[, row_id := NULL]    
    results=results[to_include==T]
    results[,to_include:=NULL]
  }
  results_daily = results[,.(outcome=sum(outcome,na.rm =T),trades=.N),date]
  results_daily = merge(results_daily, results[,.(n_sold=.N),sell_date], 
                        all=T, by.x='date',by.y='sell_date')
  results_daily[,n_sold:=ifelse(is.na(n_sold),0,n_sold)]
  results_daily[,outcome:=ifelse(is.na(outcome),0,outcome)]
  results_daily[,trades:=ifelse(is.na(trades),0,trades)]
  setorder(results_daily, date)
  results_daily[,drawdown:=drawdown(outcome)]
  results_daily[,drawdown_i:=cumsum(drawdown==0)]
  results_daily[,n_held:=cumsum(trades-n_sold)]
  results_overall= results_daily[,.(
      average=round(sum(outcome)/sum(trades),3),
      drawdown=round(min(drawdown,na.rm=T),1),
      total=round(sum(outcome),1),
      trades=sum(trades),
      days_traded=length(unique(ifelse(trades>0,date,NA))),
      max_held=max(n_held)
    ), year(date) ]%>%
    merge(results[,.(avg_days_held=mean(days_held),
                     stocks_traded=length(unique(symbol))),
                  year(date)])
  with(results_daily, plot(date,drawdown,type='l', 
                           ylim=c(-10,max(cumsum(outcome) )),
                           col='orange'))
  with(results_daily, points(date,cumsum(outcome),type='l'))
  with(results_daily, points(date,n_held/5,type='l',col="blue"))
  abline(v = seq(as.Date("2000-01-01"), as.Date("2030-12-31"), by = "year"),lty=2,col='gray')
  abline(v = seq(as.Date("2000-01-01"), as.Date("2030-12-31"), by = "month"),lty=3,col='lightgray')
  max_drawdown_days = max(results_daily[,.(datediff=max(date)-min(date)),drawdown_i][,datediff])
  print(results_overall[,
                        .(yr_total_per_held=mean(total[days_traded>10])/ifelse(hold_max,hold_max,1), 
                           avg_trade=sum(total)/sum(trades),
                           drawdown=min(drawdown), 
                           total_per_drwdn=sum(total)/abs(min(drawdown)), 
                           drawdown_days=max(max_drawdown_days),
                           days_traded=sum(days_traded),
                           max_held=max(max_held))])
  print(results_overall[order(year)])
  return(results_daily)
}

performance_features=function(dataset){
  dataset[,lead1date:= shift(date,1,type='lead'),symbol]
  dataset[,lead1sell_rally:= shift(sell_rally,1,type='lead'),symbol]
  dataset[,lead1sell_rallydate:= shift(sell_rally_date,1,type='lead'),symbol]
  
  rally(dataset,
        sell_rule=function(dat){dat$lag1close<dat$lag1low+.2*(dat$lag1high-dat$lag1low) },
        varname='sell_lowclose',
        sell_close=F)
  dataset[,lead1sell_lowclose:= shift(sell_lowclose,1,type='lead'),symbol]
  dataset[,lead1sell_lowclosedate:= shift(sell_lowclose_date,1,type='lead'),symbol]
}
