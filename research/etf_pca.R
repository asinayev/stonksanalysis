

prices[,fullday_delta:=close/lag1close]
key_etfs = prices[!grepl('short|bear|inverse', name, ignore.case = T),
                  .(v2022 = mean(ifelse(year(date)==2022, volume, NA),na.rm=T),
                    vol2022 = sd(ifelse(year(date)==2022, fullday_delta, NA),na.rm=T),
                    years = length(unique(year(date)))),
       symbol]
key_etf_wide = prices[symbol %in% key_etfs$symbol & !is.na(fullday_delta) ] %>%
  dcast(date~symbol, value.var='fullday_delta',fun.aggregate = mean) 

max_not_one = function(x)max(x[x!=1])
cols_to_exclude=c('date')
max_=1
etf_corrs = data.frame(key_etf_wide)[,!names(key_etf_wide) %in% cols_to_exclude]%>% 
  cor(use='pairwise.complete') 
while(max_>.9){
  etf_corrs_incl = etf_corrs[!colnames(etf_corrs) %in% cols_to_exclude,!colnames(etf_corrs) %in% cols_to_exclude]
  most_correlated_var_i = which.max(apply(etf_corrs_incl,1,max_not_one))
  corrs_to_most_correlated = etf_corrs_incl[,most_correlated_var_i]
  cols_to_exclude=c(cols_to_exclude,colnames(etf_corrs_incl)[corrs_to_most_correlated>.9 & corrs_to_most_correlated<1])
  print(length(c('excluding:',cols_to_exclude)))
  max_=max(ifelse(etf_corrs_incl[,most_correlated_var_i]==1,0,etf_corrs_incl[,most_correlated_var_i]))
  print(max_)
}
names(key_etf_wide)[!names(key_etf_wide) %in% cols_to_exclude]%>%paste(collapse = "','")



# x = key_etf_wide[year(date)<2022,.SD,.SDcols=key_etfs] %>%
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
  


c('VT','VIXM','KBWB')