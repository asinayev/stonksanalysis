

prices[,fullday_delta:=close/lag1close]
key_etfs = prices[,.(v2022 = mean(ifelse(year(date)==2022, volume, NA),na.rm=T),
          years = length(unique(year(date)))),
       symbol][v2022>1000000 & years>7,symbol]
key_etf_wide = prices[symbol %in% key_etfs & !is.na(fullday_delta) ] %>%
  dcast(date~symbol, value.var='fullday_delta',fun.aggregate = mean) %>% 
  na.omit()




x = key_etf_wide[year(date)<2022,.SD,.SDcols=key_etfs] %>% 
  prcomp(center=T,scale.=T,)

get_pca_deviations = function(dat, model, k=10){
  day_loads = predict(dat[,.SD,.SDcols=key_etfs], object = x)
  outs = data.table(scale(dat[,.SD,.SDcols=key_etfs],center=model$center, scale=model$scale) -
    day_loads[,1:k] %*% t(model$rotation[,1:k]))
  outs$date = dat$date
  outs %>% melt(id.vars='date')
}

y = key_etf_wide[year(date) == 2022] %>% 
  get_pca_deviations(model=x, k=10)
  