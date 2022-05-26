args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
  }
source("implement/imports.R", local=T)

history = sampled_data(key = POLYKEY, date = Sys.Date()-365, end_date = Sys.Date()+1, 
                       ticker_type = 'CS', details=F)

history = history[stock %in% history[!is.na(AdjClose) & !is.na(open),.N,stock][N>35, stock]]
setorder(history, stock, Date)

history[,days_around:=cumsum(!is.na(AdjClose)),stock]
history[,day_delta:= AdjClose/open]
history[!is.na(volume),
        volume_avg:= SMA(shift(volume,1,type='lag'), n = 30, ),stock ]
history[,c("lag1close", "lag2close", "lead1close"):=
          shift(AdjClose, n = c(1:2,-1), type = "lag"),stock]
history[,c("lead1open"):=
          shift(open, n = c(-1), type = "lag"),stock]
prices[,c("lag1volume"  ):=shift(volume,   n = 1, type = "lag"),symbol]
history[!is.na(AdjClose),close_running_min:= frollapply(shift(AdjClose,n=1,type='lag'), min, n = 50 ),stock ]

history %>% 
  subset(Date == max(Date) & day_delta<.925 & (AdjClose<low*1.02 | AdjClose<close_running_min*1.02)  
         & lag1volume>75000 & AdjClose>5,  
         select=c('stock','AdjClose','volume','low','open','close_running_min','lag1close')) %>%
  dplyr::mutate( symbol=stock, action='BUY', 
                 strike_price=pmin(pmax(low, close_running_min), open*.8), 
                 order_type='LOC', time_in_force='') %>%
  write_strat(strat_name='bandlong')

history %>% 
  subset(Date == max(Date) & volume_avg>1000000 & lag1close>30 &
           AdjClose<high/1.04 & AdjClose<(low+.1*(high-low)),  
         select=c('stock','AdjClose','volume','high','low','open','close_running_min','lag1close')) %>%
  dplyr::mutate( symbol=stock, action='BUY', 
                 strike_price=pmin( low+.04*(high-low), high/1.05), 
                 order_type='LOC', time_in_force='') %>%
  write_strat(strat_name='lowclose_big')