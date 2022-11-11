

prices[,fullday_delta:=close/lag1close]
key_etfs = prices[,
                  .(v2021 = mean(ifelse(year(date)==2021, volume, NA),na.rm=T),
                    vol2021 = sd(ifelse(year(date)==2021, fullday_delta, NA),na.rm=T),
                    N2021 = length(na.omit(ifelse(year(date)==2021,fullday_delta,NA))),
                    years = length(unique(year(date)))),
       symbol][v2021>100000 & N2021>50 & symbol!='DWFI' & years>7]
key_etf_wide = prices[symbol %in% key_etfs$symbol & !is.na(fullday_delta) ] %>%
  dcast(date~symbol, value.var='fullday_delta',fun.aggregate = mean) 

max_not_one = function(x)max(x[x!=1])
cols_to_exclude=c('date')
max_=1
etf_corrs = data.frame(key_etf_wide)[,!names(key_etf_wide) %in% cols_to_exclude]%>% 
  cor(use='pairwise.complete') %>%
  data.table
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



x = key_etf_wide[year(date)<2022,.SD,.SDcols=key_etfs$symbol] %>%
  na.omit() %>%
  princomp(center=T,scale.=T,na.action=na.pass)


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


x = psych::principal(key_etf_wide[,.SD,.SDcols=key_etfs$symbol] 
                     ,nfactors=10,rotate='promax')
data.table(as.data.frame.matrix(x$loadings),keep.rownames = T)[order(RC1),.(rn,RC1)]

#PROMAX#
key_etfs = c('safe large cap'='MGK', 'large govnt bonds'='IEF', 'oil'='USO', 
  'gold'='RING', 'china'='FXI', 'tech'='ARKK', 
  'near term bonds'='SPSB', 'small cap value'='AVUV', 'utilities'='XLU')
data.table(key_etf = key_etfs[apply(etf_corrs[,.SD,.SDcols=key_etfs],1,which.max)],
           etf_name = names(etf_corrs))

