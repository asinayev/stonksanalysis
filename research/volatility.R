# Buy tecl, soxl, and sso when vixy spikes 
spy_prices=prices[symbol%in%c('TECL')]
spy_prices=merge(spy_prices, prices[symbol=='VIXY',.(date, vix_entry=close/open > 1.015)])
spy_prices[vix_entry==T]%>%
  with(performance(date,lead1sellrally/lead1open-1,lead1sellrallydate-date,symbol))
