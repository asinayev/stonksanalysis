args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)

prices = fread("/tmp/fx_prices.csv")

prices = prices[, .SD[1], by=.(stock, Date)][
  ,.(symbol,date, open, high, low, volume, close, market_cap=NA)]

lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)

prices[order(day_drop_norm, decreasing=F)][
  date==max(date, na.rm=T) & low<lag1close*.98] %>%
  dplyr::mutate( action='BUY', 
                 order_type='Adaptive',
                 time_in_force='DAY') %>%
  head(1) %>%
  write_strat(strat_name='revert_fx.csv')
