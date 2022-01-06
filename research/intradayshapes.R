setwd("stonksanalysis")
source("prep_data.R", local=T)
source("crossover_strategy.R", local=T)
source("polygon.R", local=T)

library(tidyquant)

POLYKEY = Sys.getenv('POLYGONKEY')

extract_features_for_day = function(daydat){
  daydat=daydat[order(DateTime)]
  morndat = daydat[(hour(DateTime)*60+minute(DateTime)) %between% c(9*60+30,12*60)]
  noondat = daydat[(hour(DateTime)*60+minute(DateTime)) %between% c(12*60+30,16*60)]
  if(nrow(morndat)<12 || nrow(noondat)<2){
    return(list('polyfit1'=0,'polyfit2'=0,'polyfit3'=0,'polyfit4'=0,'sigma'=0,'N'=0,'delta'=0))
    }
  polyfit = poly(morndat$TimeStamp,4)
  outcome = morndat$Open/morndat$Open[1]-1
  lm0=lm(outcome~0+polyfit)
  if(length(lm0$coefficients)!=4){
    return(list('polyfit1'=0,'polyfit2'=0,'polyfit3'=0,'polyfit4'=0,'sigma'=0,'N'=0,'delta'=0))
  }
  return(as.list(c(lm0$coefficients, 
                   "sigma"=summary(lm0)$sigma, 
                   'N'=daydat[,.N],
                   'delta'= noondat[.N,AdjClose]/noondat[1,Open] )))
}

get_features_for_stock = function(stockname){
  stockdat = rbind(
    stock_day(
      stockname = stockname,
      start_date='2018-01-01',
      end_date='2019-12-31',
      key=POLYKEY,
      interval='minute',
      interval_len=10),
    stock_day(
      stockname = stockname,
      start_date='2020-01-01',
      end_date='2021-12-31',
      key=POLYKEY,
      interval='minute',
      interval_len=10), 
    fill=T
    )
  stockdat[,date:=as.Date(DateTime)]
  setorder(stockdat, stock, DateTime)
  daily_features = stockdat[,extract_features_for_day(.SD),.(date,stock)]
  return(daily_features)
}

splits = 10
system.time(
  stockdat <- fundamentals[Volume>1000]$Symbol %>%
    unique %>% 
    parallel::mclapply(
    # lapply(  
      get_features_for_stock
      , mc.cores = splits
    ) %>% rbindlist(fill=T)
)

setorder(daily_features, stock, date)
daily_features[N==0, c('polyfit1','polyfit2','polyfit3','polyfit4','sigma','delta'):=NA]
daily_features[,c("lag1_delta"):=shift(delta, n = c(1), type = "lag"),stock]
daily_features[,c("lag1_polyfit1"):=shift(polyfit1, n = 1, type = "lag"),stock]
daily_features[,c("lag1_polyfit2"):=shift(polyfit2, n = 1, type = "lag"),stock]
daily_features[,c("lag1_polyfit3"):=shift(polyfit3, n = 1, type = "lag"),stock]
daily_features[,c("lag1_polyfit4"):=shift(polyfit4, n = 1, type = "lag"),stock]
daily_features[,c("lag1_sigma"):=shift(sigma, n = 1, type = "lag"),stock]
daily_features = daily_features[stock %in% daily_features[!is.na(delta),.N,stock][N>300, stock]]


daily_features[!is.na(lag1_delta) & !is.na(lag1_polyfit1),
       corr_lag_polyfit1:=
         runCor( lag1_delta, lag1_polyfit1, 50),
       stock]
daily_features[!is.na(lag1_delta) & !is.na(lag1_polyfit2),
               corr_lag_polyfit2:=
                 runCor( lag1_delta, lag1_polyfit2, 50),
               stock]
daily_features[!is.na(lag1_delta) & !is.na(lag1_polyfit3),
               corr_lag_polyfit3:=
                 runCor( lag1_delta, lag1_polyfit3, 50),
               stock]
daily_features[!is.na(lag1_delta) & !is.na(lag1_polyfit4),
               corr_lag_polyfit4:=
                 runCor( lag1_delta, lag1_polyfit4, 50),
               stock]
daily_features[!is.na(lag1_delta) & !is.na(lag1_sigma),
       corr_lag_sigma:=
         runCor( lag1_delta, lag1_sigma, 50),
       stock]

daily_features_merged = merge(daily_features,fundamentals, by.x = 'stock',
                              by.y = 'Symbol', all.x=T)
daily_features_merged = daily_features_merged[polyfit1> .02 & sigma>.005]
yr = 2021

lm0=lm(delta~
        polyfit1*corr_lag_polyfit1
       +polyfit2*corr_lag_polyfit2
       +polyfit3*corr_lag_polyfit3
       +polyfit4*corr_lag_polyfit4
       +sigma*corr_lag_sigma
     , 
     daily_features_merged[year(date)==yr-1 ])
summary(lm0)

daily_features_merged[,pred_delta:=NULL]
daily_features_merged[year(date)==yr, pred_delta:=predict(lm0,.SD)]
daily_features_merged[year(date)==yr, .(mean(delta,na.rm=T),.N),round(pred_delta,2)][order(round)]
daily_features_merged[year(date)==yr & pred_delta>1.01, .(mean(delta,na.rm=T),.N)]
daily_features_merged[year(date)==yr & pred_delta<.99, .(mean(delta,na.rm=T),.N)]

