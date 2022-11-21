# Long tecl, soxl, labu if they fall when vixy rises .5%. Sell when they're up 2.5%
# perf drawdown days_traded
# 1: 0.02141667       -4         923
# year average drawdown total trades days_traded avg_days_held stocks_traded
# 1: 2011   0.023     -0.4   0.6     26          26      4.038462             1
# 2: 2012   0.017     -0.9   1.7    103          79      5.514563             2
# 3: 2013   0.018     -0.1   2.1    116          70      3.741379             2
# 4: 2014   0.022     -0.2   2.8    124          77      4.137097             2
# 5: 2015   0.015     -1.3   2.2    148          74      5.344595             3
# 6: 2016   0.009     -4.0   1.6    188          87      5.739362             3
# 7: 2017   0.024     -0.2   3.6    153          83      4.503268             3
# 8: 2018   0.000     -3.7  -0.1    216          92      6.361111             3
# 9: 2019   0.018     -2.6   3.2    176          83      4.170455             3
# 10: 2020   0.039     -2.3   6.7    171          88      5.040936             3
# 11: 2021   0.023     -1.2   5.0    212         103      4.429245             3
# 12: 2022   0.049     -0.8   6.9    142          61      4.978873             3
spy_prices=prices[symbol%in%c('TQQQ','SOXL','LABU','WANT','UGE')]
spy_prices=merge(spy_prices, prices[symbol%in%c('VIXY','VIXM','VXX','VXZ'),
                                    .(vix_entry=any(close>open*1.0025,na.rm = T )),date],by='date')
setorder(spy_prices, symbol, date)

spy_prices[vix_entry==T & close<lag1high & sell_rally_day>1 ]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol))
