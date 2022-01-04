stockdat = rbind(stock_day('BANR','2020-01-01','2021-01-01',POLYKEY, interval='minute',10),
                 stock_day('JD','2020-01-01','2021-01-01',POLYKEY, interval='minute',10),
                 stock_day('ODP','2020-01-01','2021-01-01',POLYKEY, interval='minute',10),
                 stock_day('OGEN','2020-01-01','2021-01-01',POLYKEY, interval='minute',10),fill=TRUE)
stockdat = rbind(stock_day('BANR','2021-01-01','2022-01-01',POLYKEY, interval='minute',10),
                 stock_day('JD','2021-01-01','2022-01-01',POLYKEY, interval='minute',10),
                 stock_day('VCTR','2021-01-01','2022-01-01',POLYKEY, interval='minute',10),
                 stock_day('FDX','2021-01-01','2022-01-01',POLYKEY, interval='minute',10))
stockdat[,date:=as.Date(DateTime)]
setorder(stockdat, stock, TimeStamp)

extract_features = function(daydat){
  if(nrow(daydat)<20){return(as.list(rep(0,7)))}
  daydat = daydat[(hour(DateTime)*60+minute(DateTime)) %between% c(9*60+30,16*60)]
  daydat=daydat[order(DateTime)]
  polyfit = poly(daydat$TimeStamp,4)
  outcome = daydat$Open/daydat$Open[1]-1
  lm0=lm(outcome~0+polyfit)
  return(as.list(c(lm0$coefficients, 
                   "sigma"=summary(lm0)$sigma, 
                   'N'=daydat[,.N],
                   'delta'=outcome[length(outcome)])))
}

daily_features = stockdat[,extract_features(.SD),.(date,stock)][N>10]
setorder(daily_features, stock, date)
daily_features[,c("lead1delta"):=shift(delta, n = 1, type = "lead"),stock]
daily_features[,c("lag1_polyfit1"):=shift(polyfit1, n = 1, type = "lag"),stock]
daily_features[,c("lag1_sigma"):=shift(sigma, n = 1, type = "lag"),stock]

daily_features[!is.na(delta) & !is.na(lag1_polyfit1),
       corr_lag_polyfit1:=
         runCor( delta, lag1_polyfit1, 7),
       stock]
daily_features[!is.na(delta) & !is.na(lag1_sigma),
       corr_lag_delta:=
         runCor( delta, lag1_sigma, 7),
       stock]


pca = prcomp(y[!is.na(lead1delta),.(polyfit1,polyfit2,polyfit3,polyfit4,sigma,lead1delta)], scale = T)
data.frame(predict(pca, y)) %>% with(plot(PC1,PC2))
y[stock=='JD',.(polyfit1,polyfit2,polyfit3,polyfit4,sigma,lead1delta)]%>%cor(use='complete')
pca
