# Long tecl, soxl, labu if they fall when vixy rises .5%. Sell when they're up 2.5%
spy_prices=prices[symbol%in%c('TECL','SOXL','LABU')]
spy_prices=merge(spy_prices, prices[symbol=='VIXY',.(date, vix_entry=(close>open*1.005))])
spy_prices[vix_entry==T & close<open]%>%
  with(performance(date,lead1sellupup/lead1open-1,lead1sellupupdate-date,symbol))
