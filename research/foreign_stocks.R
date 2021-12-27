require(tidyquant)
require(data.table)
require(rpart)
require(ggplot2)

fundamentals = fread("stonksanalysis/other_datasources/nasdaq_screener_1638810601422.csv") # from https://www.nasdaq.com/market-activity/stocks/screener
fundamentals = fundamentals[sample(nrow(fundamentals))]

splits = 16
system.time(
  prices <- fundamentals$Symbol %>%
    unique %>% split(1:splits) %>%
    parallel::mclapply(tq_get,
                       from=Sys.Date()-6*365,
                       to=Sys.Date()+2,
                       mc.cores = splits
    ) %>%
    rbindlist(fill=T)
)

prices = prices[symbol %in% prices[!is.na(close) & !is.na(open),.N,symbol][N>365, symbol]]
setorder(prices, symbol, date)
prices[,c("lag1close", "lag2close"):=shift(close, n = 1:2, type = "lag"),symbol]
prices[,c("lag1open",  "lag2open" ):=shift(open,  n = 1:2, type = "lag"),symbol]
prices[,c("lag1high",  "lag2high" ):=shift(high,  n = 1:2, type = "lag"),symbol]
prices[,c("lag1low",   "lag2low"  ):=shift(low,   n = 1:2, type = "lag"),symbol]
prices[,day_delta:= close/open]
prices[,day_fall:= low/open]
prices[,day_rise:= high/open]
prices[,night_delta:= open/lag1close]
prices[!is.na(volume),volume_avg:= SMA(shift(volume,1,type='lag'), n = 30, ),symbol ]
prices[,c("lag1_day_delta",    "lag2_day_delta"   ):=shift(day_delta,    n = 1:2, type = "lag"),symbol]
prices[,c("lag1_night_delta",  "lag2_night_delta" ):=shift(night_delta,  n = 1:2, type = "lag"),symbol]
prices[,c("lag1_day_fall",    "lag2_day_fall"   ):=shift(day_fall,    n = 1:2, type = "lag"),symbol]
prices[,c("lag1_day_rise",    "lag2_day_rise"   ):=shift(day_rise,    n = 1:2, type = "lag"),symbol]


prices[!is.na(lag1_day_delta) & !is.na(lag1_night_delta),
       lagging_corr:=
         runCor( lag1_day_delta, lag1_night_delta, 364),
       symbol]

prices[!is.na(lag1_day_delta) & !is.na(lag2_day_rise),
       lagging_corr2:=
         runCor( lag1_day_delta, lag2_day_rise, 7),
       symbol]
prices[!is.na(lag1_day_delta) & !is.na(lag1_night_delta),
       lagging_corr3:=
         runCor( lag1_day_delta, lag2_day_delta, 7),
       symbol]
prices[!is.na(lag1_day_delta) & !is.na(lag1_night_delta),
       lagging_corr4:=
         runCor( lag1_day_delta, lag2_day_fall, 7),
       symbol]

IS = prices[date>(Sys.Date()-365) & 
              log(volume_avg*lag1close+1) > 15]
lm1 = lm(day_delta~
           lagging_corr4*lag1_day_fall +
           lagging_corr3*lag1_day_delta +
           lagging_corr2*lag1_day_rise,
     IS
   )

# Determine correct range to use for volume (it changes over time)
# plot_data = prices_metadata[, .(out = min(1.1, mean(day_delta,na.rm=T)), .N),
#                             .(pred_change = round(night_delta,2),
#                               lagging_corr = round(lagging_corr,1),
#                               year(date),
#                               volume = round(log(volume_avg+1)))
#                             ][N>30 & volume>8]
# plot_data = prices_metadata[, .(out = min(1.1, mean(day_delta,na.rm=T)), .N),
#                             .(pred_change = round(lag_day_delta,2),
#                               lagging_corr = round(lagging_corr2,1),
#                               year(date),
#                               volume = round(log(volume_avg+1)))
# ][N>30 & volume>8]
#
# ggplot(plot_data[year>2015 & volume %between% c(7,15)], aes(x=pred_change,
#                       y=lagging_corr, color=out, size=N))+
#   geom_point(alpha=.9) +
#   scale_color_gradient2(midpoint = 1, low = "red", mid ="white", high = "blue", space = "Lab" )+
#   geom_hline(yintercept=0) +
#   geom_vline(xintercept=1) +
#   facet_grid(volume~year)

# prices_metadata[,mean(lagging_corr,na.rm=T),round(log(volume_avg+1))][order(round)]
# Whitelist Log volume 10-13 seems good

# prices_metadata[,                    .(mean(lagging_corr,na.rm=T),.N),Sector][order(V1)]
# prices_metadata[log(volume_avg+1)<11,.(mean(lagging_corr2,na.rm=T),.N),Sector][order(V1)]
# prices_metadata[log(volume_avg+1)<12 & year(date)>2019,.(mean(lagging_corr,na.rm=T),.N),Sector][order(V1)]
# Whitelist: Consumer Durables, Consumer Non-Durables, Consumer Services, Technology and recently, Finance

# prices_metadata[log(volume_avg+1)<11 & Sector %in% c('Consumer Durables', 'Consumer Non-Durables','Consumer Services', 'Technology', 'Finance'),
#                 .(mean(abs(lagging_corr2),na.rm=T),.N),Country][order(V1)]

subsample = prices[log(volume_avg+1) %between% c(10,13)]

subsample[night_delta<.97 & lagging_corr< -.4,
          .(mean(day_delta,na.rm=T),.N), year(date)][order(year)]
subsample[date==max(date, na.rm=T) & lagging_corr< -.4,
          .(date, symbol, close, 
            buy = trunc(close*97,3)/100 , sell = (trunc(close*103,3)+1)/100)] %>%
  fwrite('/tmp/correlated_stocks.csv')

prices[date==max(date, na.rm=T) & 
         log(volume_avg*lag1close+1)>15 & 
         (predict(lm1, prices) < quantile(predict(lm1, IS), .0005, na.rm=T)) ,
       .(date, symbol, close)] %>%
  fwrite('/tmp/predicted_short_stocks.csv')
