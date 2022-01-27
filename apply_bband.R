require(tidyquant, quietly = T)
require(data.table, quietly = T)

args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){args='~/stonksanalysis'}
setwd(args[1])
source("polygon.R", local=T)
POLYKEY = Sys.getenv('POLYGONKEY')

stopifnot(POLYKEY!='')

current_moves = "https://api.polygon.io/v2/snapshot/locale/us/markets/stocks/tickers?apiKey=%s" %>%
  sprintf(POLYKEY) %>%
  hit_polygon

history = current_moves$tickers$ticker %>%
  parallel::mclapply(
    stock_history,
    start_date = Sys.Date()-365, end_date = Sys.Date()+1, key = POLYKEY, 
    print=F, check_ticker=F,mc.cores = 16) %>% 
  rbindlist(use.names=TRUE, fill=T)

history = history[stock %in% history[!is.na(AdjClose) & !is.na(open),.N,stock][N>35, stock]]
setorder(history, stock, Date)

history[,day_delta:= AdjClose/open]
history[!is.na(volume),
        volume_avg:= SMA(shift(volume,1,type='lag'), n = 30, ),stock ]
history[,c("lag1close", "lag2close", "lead1close"):=
          shift(AdjClose, n = c(1:2,-1), type = "lag"),stock]
history[,c("lead1open"):=
          shift(open, n = c(-1), type = "lag"),stock]
history[!is.na(AdjClose),close_running_min:= frollapply(AdjClose, min, n = 50 ),stock ]
history[(AdjClose<low*1.01 | AdjClose==close_running_min) & day_delta<.85 & volume_avg*lag1close>100000  & volume_avg*lag1close<10000000, 
       .(mean(lead1open/AdjClose, na.rm=T), median(lead1open/AdjClose,na.rm=T),.N)]


history %>% 
  subset(Date == max(Date) & day_delta<.975 & open/lag1close> 1.05  
         & volume_avg*lag1close>100000  & volume_avg*lag1close<500000,  
         select=c('stock','AdjClose','volume','low','open','close_running_min')) %>%
  dplyr::mutate( symbol=stock, action='BUY', 
                 strike_price=open*.95, 
                 order_type='LOC', time_in_force='') %>%
  fwrite('/tmp/updown.csv')

history %>% 
  subset(Date == max(Date) & day_delta<.925 & (AdjClose<low*1.02 | AdjClose<close_running_min*1.02)  
         & volume_avg*lag1close>100000  & volume_avg*lag1close<10000000,  
         select=c('stock','AdjClose','volume','low','open','close_running_min')) %>%
  dplyr::mutate( symbol=stock, action='BUY', 
                 strike_price=pmin(pmax(low, close_running_min), open*.85), 
                 order_type='LOC', time_in_force='') %>%
  fwrite('/tmp/bandlong.csv')


