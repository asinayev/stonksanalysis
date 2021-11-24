install.packages("googlesheets4")
install.packages('plyr')
install.packages('rpart.plot')
setwd("stonksanalysis")
source("polygon.R", local=T)

# Sys.setenv(POLYGONKEY= '')
POLYKEY = Sys.getenv('POLYGONKEY')
library(tidyquant)
library(data.table)
library(rpart)

pull_indicators = function(stock,date, key){
  if(WEEKDAY(date)==2){
    prev_date = as.Date(date)-3
  } else {
    prev_date = as.Date(date)-1
  }
  stock_dat = stock_day(stock,prev_date,date,key = key)
  if(nrow(stock_dat)<50){
    return(stock_dat[1,.(stock, EMA_5=NA, EMA_30=NA, bought = NA, sold=NA, prev_day_volume=NA)])
  } else {
    return(stock_dat)
  }
  # agg_dat = tq_get(stock,from=prev_date,to=as.Date(date)+1)
  # prev_day = agg_dat[agg_dat$date==prev_date,]
  # current_day = agg_dat[agg_dat$date==date,]
  # # x[DateTime<as.POSIXct("2021-11-12 09:46:00", tz = 'EST')]
  # stock_dat$EMA_5 = EMA(stock_dat$Open, n = 5)
  # stock_dat$EMA_30 = EMA(stock_dat$Open, n = 30)
  # stock_dat[DateTime==as.POSIXct(paste(date,"09:45:00"), tz = 'EST'),
  #           .(stock, EMA_5, EMA_30, price945 = AdjClose,
  #             prev_day_volume = prev_day$volume,
  #             prev_day_open = prev_day$open,
  #             prev_day_close= prev_day$close,
  #             day_open = current_day$open,
  #             day_close = current_day$close)]
}

# pull_daily_indicators = function(stock,date_){
#   if(WEEKDAY(date_)==2){
#     prev_date = as.Date(date_)-3
#   } else {
#     prev_date = as.Date(date_)-1
#   }
#   agg_dat = tq_get(stock,from=prev_date,to=as.Date(date_)+1) %>% data.table
#   prev_day = agg_dat[date==prev_date,]
#   current_day = agg_dat[date==date_,]
#   current_day[,.(
#     prev_day_volume = prev_day$volume,
#     prev_day_open = prev_day$open,
#     prev_day_close= prev_day$close,
#     day_open = open,
#     day_close = close)]
# }


# get_volume = function(stocklist, end_date=today(), day_range=14){
#   system.time(stockdat <- parallel::mclapply(unique(stocklist),
#                                              tq_get,
#                                              from=end_date-day_range,
#                                              to=end_date,
#                                              mc.cores = 8))
#   stockdat = rbindlist(stockdat[lapply(stockdat, is.data.frame)%>%unlist])
#   stockdat[,.(volume = mean(volume*close)),symbol]
# }


# earnings = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Q9KSUuVPVaCFX5DURsna7ZgI0hDFRKO-g0daA5hPKcQ/edit#gid=1003635395",
#                                      sheet = 'Earnings')
# 
# earnings = merge(data.table(earnings),
#                  get_volume(earnings$Symbol),
#                  by.x = 'Symbol',  by.y = 'symbol')
# earnings[, trade_date:=as.Date(ifelse(Time=='bmo', as.Date(Date),
#                                       ifelse(WEEKDAY(Date)==6,as.Date(Date)+3, as.Date(Date)+1  ) ))]

earnings2 = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Q9KSUuVPVaCFX5DURsna7ZgI0hDFRKO-g0daA5hPKcQ/edit#gid=1003635395",
                                            sheet = 'Earnings2')

polytickers = "https://api.polygon.io/v3/reference/tickers?type=CS&market=stocks&active=true&sort=ticker&order=asc&limit=1000&apiKey=%s" %>%
  sprintf(POLYKEY) %>%
  hit_polygon
allresults = polytickers$results
while(!is.null(polytickers$next_url)){
  Sys.sleep(12)
  newlink = sprintf('%s&apiKey=%s',polytickers$next_url, POLYKEY)
  polytickers = hit_polygon(newlink)
  allresults = rbind(allresults, polytickers$results)
}

avail_earnings = data.table(earnings2)[Symbol %in% allresults$ticker &
                            Time %in% c('amc','bmo')]
avail_earnings = avail_earnings[sample(nrow(avail_earnings))]

ping_row = function(x){
  Sys.sleep(12)
  pull_indicators(stock = x$Symbol,date = x$`Trade Date`, key = POLYKEY)
}

system.time(sampled_results <- plyr::adply(avail_earnings, 1,ping_row))
retry_results = plyr::adply(sampled_results[is.na(day_close)], 1,ping_row)



sampled_results2 = data.table(sampled_results)[Time %in% c('amc','bmo')]
sampled_results2 = merge(sampled_results2,
                      fundamentals[,c('Symbol','Market Cap', 'Country', 'IPO Year', 'Volume', 'Sector', 'Industry')],
                      by = 'Symbol',
                      all.x=T )

# sampled_results2[,EMA_5:=EMA_5/bought]
# sampled_results2[,EMA_30:=EMA_30/bought]
sampled_results2[,EMA_ratio:=Open/`Previous Close`-1]
sampled_results2[,EMA_ratio_sq:=EMA_ratio^2]
sampled_results2[,cap_num:=log(as.numeric(as.character(`Market Cap(M)`))+1)]
sampled_results2[,vol_num:=log(as.numeric(`Previous Volume`*Close)+1)]
# sampled_results2[,ipo_yr:=as.numeric(`IPO Year`)]
# sampled_results2[,money_volume:=log(prev_day_volume*prev_day_close+1)]
# sampled_results2[,sold:=sold/bought]
sampled_results2[,sold:=Close/Open-1]
sampled_results2[,sold_profit:=sold>0]
sampled_results2[,trade_date:=`Trade Date`]
sampled_results2

m1 = rpart(sold~EMA_ratio+vol_num, 
           data=sampled_results2[trade_date>as.POSIXct('2021-01-01 00:00:00')], 
           control = rpart.control(minbucket =  500, cp=.00000001) )
rpart.plot::rpart.plot(m1)
sampled_results2[,.(mean(sold, na.rm=T), median(sold, na.rm=T), mean(sold>0, na.rm=T),.N),
                 .(predict(m1, sampled_results2)>.01, trade_date>as.POSIXct('2021-01-01 00:00:00'))]

sampled_results2[EMA_ratio<.6,mean(sold),round(log(cap_num))] %>% plot

sampled_results2[EMA_ratio %between% c(-.02,-.01) & cap_num>5 & Date<'2021-01-01',
                 .(mean(sold), median(sold), mean(sold>0))]

with(sampled_results2[EMA_ratio %between% c(-.5,.5),.(median(sold,na.rm=T), .N),.(round(EMA_ratio/5,2)*5,trade_date>as.POSIXct('2021-01-01 00:00:00'))],
     plot(round,V1,cex=log(N)/2,pch=as.numeric(trade_date))
)
abline(h=0)
