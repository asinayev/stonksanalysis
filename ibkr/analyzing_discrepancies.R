require('data.table')
require('googlesheets4')
IBKR_trades = fread("~/U6840690_U6840690_20210324_20220323_AF_626854_b29a4d2a9d49205397b79b7002076e92.csv")
my_historical <- read_sheet('https://docs.google.com/spreadsheets/d/1RIwz2xXxLp6fm-RtPXyyujs8wOdLLVYV0k_3Uw-_phw') %>%data.table

move_ids = function(quantities){
  outs = rep(0,length(quantities))
  outstanding = cumsum(quantities)
  move_id=0
  for(i in 1:length(quantities)){
    outs[i]=move_id
    if(outstanding[i]==0){move_id=move_id+1}
  }
  return(outs)
}

setorder(IBKR_trades, Symbol, DateTime)
IBKR_trades[,move_id:=move_ids(Quantity ),Symbol]
IBKR_moves = IBKR_trades[,.(date=as.character(min(TradeDate)),
               prop_diff=sum(Proceeds)/sum(ifelse(`Open/CloseIndicator`=='O',abs(TradeMoney), 0))+1,
               dollar_diff=sum(Proceeds),
               fee=sum(IBCommission),
               settled=sum(Quantity)==0,
               bought=mean(ifelse(Quantity>0, TradePrice, NA),na.rm=T),
               sold  =mean(ifelse(Quantity<0, TradePrice, NA),na.rm=T)),
            .(Symbol,move_id)]
IBKR_days = IBKR_moves[,.SD[1],.(Symbol,date)]

comparison_set = merge(IBKR_days,
  my_historical[,.(Stock, date_str=as.character(Date), Bought, Sold, Strategy, Diff)], 
  by.x=c('Symbol','date'),
  by.y=c('Stock','date_str'), all = T)

comparison_set[!is.na(fee),sum(fee)]
