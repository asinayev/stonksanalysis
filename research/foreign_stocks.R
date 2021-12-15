require(tidyquant)
require(data.table)
require(rpart)

fundamentals = fread("stonksanalysis/other_datasources/nasdaq_screener_1638810601422.csv") # from https://www.nasdaq.com/market-activity/stocks/screener
fundamentals = fundamentals[sample(nrow(fundamentals))]

prices <- fundamentals$Symbol %>%
  unique %>% split(1:8) %>%
  parallel::mclapply(tq_get,
                     from=Sys.Date()-2*365,
                     to=Sys.Date()+2,
                     mc.cores = 8
  ) %>%
  rbindlist(fill=T)

prices = prices[symbol %in% prices[!is.na(close) & !is.na(open),.N,symbol][N>365, symbol]]
prices = prices[order(symbol,date)]
prices[,c("lag1close", "lag2close"):=shift(close, n = 1:2, type = "lag")]
prices[,"lag1open":=shift(open, n = 1, type = "lag")]
prices[,abroad_delta:= lag1open/lag2close]
prices[,us_delta:= lag1close/lag1open]
prices[,day_premarket:= open/lag1close]
prices[,day_delta:= close/open]
prices[!is.na(volume),volume_avg:= SMA(shift(volume,1,type='lag'), n = 30, ) ]

prices[!is.na(us_delta) & !is.na(abroad_delta),
       lagging_corr:=
         runCor( us_delta, abroad_delta, 365),
       symbol]

prices_metadata = merge(prices,fundamentals,by.x = 'symbol', 
                        by.y = 'Symbol',all.x = T)

# prices_metadata[,mean(lagging_corr2,na.rm=T),round(log(volume_avg+1))][order(round)]
# Whitelist Log volume < 11 seems good

# prices_metadata[,                    .(mean(lagging_corr2,na.rm=T),.N),Sector][order(V1)]
# prices_metadata[log(volume_avg+1)<11,.(mean(lagging_corr2,na.rm=T),.N),Sector][order(V1)]
# prices_metadata[log(volume_avg+1)<11 & year(date)>2019,.(mean(lagging_corr2,na.rm=T),.N),Sector][order(V1)]
# Whitelist: Consumer Durables, Consumer Non-Durables, Consumer Services, Technology and recently, Finance

# prices_metadata[log(volume_avg+1)<11 & Sector %in% c('Consumer Durables', 'Consumer Non-Durables','Consumer Services', 'Technology', 'Finance'),.(mean(abs(lagging_corr2),na.rm=T),.N),Country][order(V1)]

subsample = prices_metadata[
  log(volume_avg+1) %between% c(7,11)]

subsample[day_premarket<.98 & lagging_corr< -.3
          ,
          .(mean(day_delta,na.rm=T),.N), year(date)][order(year)]
subsample[date==max(date, na.rm=T) & lagging_corr< -.3,
          .(date, symbol, close, buy = trunc(close*98,3)/100 , sell = trunc(close*102,3)/100)] %>%
fwrite('/tmp/correlated_stocks.csv')

# Determine correct range to use for volume (it changes over time)
# plot_data = subsample[,
#                       .(out = mean(day_delta,na.rm=T), .N),
#                       .(day_premarket = round(day_premarket,2),
#                         lagging_corr = round(lagging_corr,1),
#                         year(date),
#                         volume = round(log(volume_avg+1)))][N>30 & volume>6]
# 
# ggplot(plot_data, aes(x=day_premarket, y=lagging_corr, color=out, size=N))+
#   geom_point(alpha=.9)+
#   scale_color_gradient2(midpoint = 1, low = "red", mid = "white",
#                         high = "blue", space = "Lab" )+
#   geom_hline(yintercept=0) +
#   geom_vline(xintercept=1) +
#   facet_grid(volume~year)
