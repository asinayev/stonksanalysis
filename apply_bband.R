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
history[!is.na(lag1close),
       c('lower','avg','upper','pctB'):= 
         data.frame(BBands(lag1close, n = 30, EMA, sd=2.5)), stock]
history[AdjClose<lower*.9 & day_delta<.85 & volume_avg*lag1close>100000, 
       .(mean(lead1open/AdjClose, na.rm=T), median(lead1open/AdjClose,na.rm=T),.N)]


history %>%
  subset(Date == max(Date) & day_delta<.925 & AdjClose<lower*.975  & volume_avg*lag1close>100000,  
         select=c('stock','AdjClose','volume','lower','open')) %>%
  dplyr::mutate( symbol=stock, action='BUY', 
                 strike_price=pmin(lower*.9, open*.85), 
                 order_type='LOC', time_in_force='', sell=0) %>%
  fwrite('/tmp/bandlong.csv')

