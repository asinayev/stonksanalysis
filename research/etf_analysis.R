require(tidyquant)
require(data.table)
require(rpart)
require(ggplot2)

setwd('~/stonksanalysis')
source("polygon.R", local=T)
source("implement/features.R", local=T)
source("research/performance.R", local=T)
POLYKEY = Sys.getenv('POLYGONKEY')

# prices=lapply(Sys.Date()-365*10:1, sampled_data, key=POLYKEY, ticker_type='ETF', details=T) %>%   
#   rbindlist(fill=T) %>%
#   dplyr::rename(symbol=stock, close=AdjClose, date=Date)
# prices=lapply(Sys.Date()-365*18:6, sampled_data, key=POLYKEY, ticker_type='INDEX', details=T) %>%   
#   rbindlist(fill=T) %>%
#   dplyr::rename(symbol=stock, close=AdjClose, date=Date) %>% rbind(prices, fill=T)
# 
# setorder(prices, symbol, date)

prices=fread("~/datasets/etf_prices_15y.csv")

lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)
rally_avg(prices,200)

# Rally ETFs
# perf drawdown days_traded
# 1: 0.02193333     -5.8        1078
# year average drawdown total trades days_traded avg_days_held stocks_traded
# 1: 2008   0.057     -0.5   3.6     62          36      4.741935            11
# 2: 2009   0.008     -0.9   1.1    140          82      4.685714            11
# 3: 2010   0.011     -0.3   1.3    117          96      3.777778             8
# 4: 2011   0.030     -0.6   4.1    140          83      4.192857            10
# 5: 2012   0.019      0.0   0.4     19          15      2.157895             8
# 6: 2013   0.031     -0.2   0.5     17          15      7.117647             3
# 7: 2014  -0.015     -1.5  -0.6     40          32      3.800000             8
# 8: 2015   0.074     -1.3   4.1     55          35      3.345455            11
# 9: 2016   0.005     -2.8   1.3    263         121      5.494297            18
# 10: 2017   0.018     -1.6   1.4     78          68      3.448718             8
# 11: 2018   0.018     -0.6   0.9     51          49      5.215686            10
# 12: 2019   0.034     -0.5   3.4    100          72      4.440000             7
# 13: 2020  -0.017     -5.8  -3.7    219         109      5.232877            28
# 14: 2021   0.024     -4.9   9.2    390         170      4.794872            34
# 15: 2022   0.032     -3.1   6.0    186          95      4.451613            28        45

setorder(prices, symbol, date)

prices[,short:=grepl('short|bear|inverse', name, ignore.case = T)]
prices[,lever:=grepl('2x|3x|leverag|ultra', name, ignore.case = T)]

prices[volume>75000 & close>7 & !short & 
         lead1sellrally/lead1open<2 & 
         close<lag1high & sell_rally_day>2 &
         ((sell_rally_avg-avg_delta)/sell_rally_avg)>.018][
           order(-lever, (sell_rally_avg-avg_delta)/sell_rally_avg,decreasing = T),head(.SD,3),date] %>%
  with(performance(date,lead1sellrally/lead1open-1,lead1sellrallydate-date,symbol))

# revert ETFs
# perf drawdown days_traded
# 1: 0.02105556     -2.7         649
# year average drawdown total trades days_traded avg_days_held stocks_traded
# 1: 2005   0.009      0.0   0.0      3           3      5.666667             2
# 2: 2006   0.013      0.0   0.0      1           1      5.000000             1
# 3: 2007   0.014      0.0   0.1     10          10      3.900000             5
# 4: 2008   0.012     -2.7   1.5    125          41      6.648000            56
# 5: 2009   0.036     -0.7   2.5     71          34      7.042254            34
# 6: 2010   0.053     -0.2   3.4     64          30      4.718750            36
# 7: 2011   0.009     -2.2   0.8     97          41      5.298969            42
# 8: 2012   0.023     -0.1   0.8     34          17      5.029412            17
# 9: 2013   0.011     -0.2   0.4     33          21      6.242424            22
# 10: 2014   0.010     -1.6   0.6     59          29      5.949153            29
# 11: 2015   0.037     -0.8   3.6     97          55      5.463918            41
# 12: 2016   0.041     -1.3   5.0    121          58      6.801653            38
# 13: 2017  -0.012     -0.6  -0.3     26          15      6.500000            17
# 14: 2018   0.015     -1.7   1.8    117          52      6.923077            45
# 15: 2019   0.041     -0.4   2.1     51          33      4.745098            20
# 16: 2020   0.022     -1.8   5.2    232          83      5.547414           108
# 17: 2021   0.026     -0.6   3.7    142          71      6.253521            61
# 18: 2022   0.019     -1.7   3.1    168          55      6.517857            71

prices[volume>75000 & close>7 & !short &
  lead1sellrally/lead1open<2 & 
    (((close-low)/(high-low))<.05 ) & 
    ((high/close) > 1.075 |
       ((running_low == low | RSI<.6) & ((avg_range/close) > .05)
       ) 
    )][order(RSI,decreasing=F),head(.SD,5),date]%>%
  with(performance(date,lead1sellrally/lead1open-1,lead1sellrallydate-date,symbol))


# perf drawdown days_traded
# 1: 0.02735294     -4.2        1298
# year average drawdown total trades days_traded avg_days_held stocks_traded
# 1: 2006   0.076      0.0   0.1      1           1      2.000000             1
# 2: 2007   0.045      0.0   0.3      6           6      2.000000             3
# 3: 2008   0.037     -1.9   3.4     91          33      5.241758            36
# 4: 2009   0.017     -0.1   0.4     24          15      3.666667            13
# 5: 2010   0.044     -0.2   6.6    149          59      4.785235            29
# 6: 2011   0.023     -2.1   4.4    195          78      4.153846            49
# 7: 2012   0.027     -0.2   1.1     39          19      3.538462            16
# 8: 2013   0.014     -0.6   1.0     75          43      4.120000            17
# 9: 2014   0.048     -0.3   2.5     52          28      5.269231            17
# 10: 2015   0.025     -1.1   4.0    163          89      4.699387            37
# 11: 2016   0.017     -4.2   4.5    263         131      5.764259            36
# 12: 2017  -0.002     -1.7  -0.2     96          75      6.687500             9
# 13: 2018   0.014     -2.9   4.9    347         146      5.645533            44
# 14: 2019   0.024     -3.3   6.9    290         147      5.548276            37
# 15: 2020   0.025     -2.0   8.1    327         129      4.758410           106
# 16: 2021   0.012     -2.2   4.3    357         163      6.302521            55
# 17: 2022   0.019     -2.2   8.4    449         136      5.031180            87         28        79         80       87
prices[,rownum:=order(lagging_corr_long, decreasing = F),date]

prices[ifelse(short, avg_volume>100000, avg_volume>500000) & close>7 & #!short &
         avg_delta_short<.975 & lagging_corr_long> .35][
           order(-rownum),head(.SD,5),date]%>%
  with(performance(date,lead1sellrally/lead1open-1,lead1sellrallydate-date,symbol))
# 