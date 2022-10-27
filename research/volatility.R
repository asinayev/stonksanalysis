# Long tecl, soxl, labu if they fall when vixy rises .5%. Sell when they're up 2.5%
spy_prices=prices[symbol%in%c('TECL','SOXL','LABU')]
spy_prices=merge(spy_prices, prices[symbol%in%c('VIXY','VIXM','VXX','VXZ'),
                                    .(vix_entry=max(close>open*1.0025 )),date],by='date')
spy_prices[vix_entry==T & close<lag1high & sell_rally_day>1 ]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol))
