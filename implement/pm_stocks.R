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
history[!is.na(AdjClose),close_running_min:= frollapply(AdjClose, min, n = 50 ),stock ]

history %>% 
  subset(Date == max(Date) & day_delta<.95 & open/lag1close> 1.05  
         & volume*lag1close>75000  & volume*lag1close<1000000  & AdjClose>5,  
         select=c('stock','AdjClose','volume','low','open','close_running_min','lag1close')) %>%
  dplyr::mutate( symbol=stock, action='BUY', 
                 strike_price=open*.9, 
                 order_type='LOC', time_in_force='') %>%
  write_strat(strat_name='updown')

history %>% 
  subset(Date == max(Date) & day_delta<.925 & (AdjClose<low*1.02 | AdjClose<close_running_min*1.02)  
         & volume_avg*lag1close>100000 & days_around>200 & AdjClose>5,  
         select=c('stock','AdjClose','volume','low','open','close_running_min','lag1close')) %>%
  dplyr::mutate( symbol=stock, action='BUY', 
                 strike_price=pmin(pmax(low, close_running_min), open*.85), 
                 order_type='LOC', time_in_force='') %>%
  write_strat(strat_name='bandlong')


