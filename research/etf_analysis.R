require(tidyquant)
require(data.table)
require(rpart)
require(ggplot2)

setwd('~/stonksanalysis')
source("polygon.R", local=T)
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
#         perf  drawdown days_traded
# 1: 0.02586667    -12.4        1078
#    year average drawdown total trades days_traded avg_days_held stocks_traded
# 1: 2008   0.074      5.9   7.2     98          36 4.193878 days            17
# 2: 2009   0.007     -0.2   1.1    152          82 4.631579 days            11
# 3: 2010   0.011     -0.1   1.3    117          96 3.777778 days             8
# 4: 2011   0.027     -0.2   4.6    173          83 4.358382 days            15
# 5: 2012   0.021      0.9   0.4     21          15 2.190476 days             8
# 6: 2013   0.031      0.7   0.5     17          15 7.117647 days             3
# 7: 2014  -0.015     -0.8  -0.6     40          32 3.800000 days             8
# 8: 2015   0.066     -0.7   5.2     79          35 2.898734 days            18
# 9: 2016   0.012     -3.6   4.0    336         121 5.529762 days            22
# 0: 2017   0.018     -0.7   1.4     79          68 3.455696 days             9
# 1: 2018   0.018     -0.5   0.9     51          49 5.215686 days            10
# 2: 2019   0.034     -0.5   3.4    100          72 4.440000 days             7
# 3: 2020   0.018    -12.4   9.6    544         109 4.922794 days            79
# 4: 2021   0.027      0.3  18.0    666         170 4.211712 days            44
# 5: 2022   0.039     -0.2  14.6    375          95 3.512000 days            45

setorder(prices, symbol, date)

prices[,short:=grepl('short|bear|inverse', name, ignore.case = T)]
prices[,lever:=grepl('2x|3x|leverag|ultra', name, ignore.case = T)]

prices[volume>75000 & close>7 & !short & !lever &
         lead1sellrally/lead1open<2 & 
         close<lag1high & sell_rally_day>2 &
         ((sell_rally_avg-avg_delta)/sell_rally_avg)>.012]%>%
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

potential_buys = prices[volume>75000 & close>7 & !short &
                          lead1sellrally/lead1open<2]
potential_buys[!is.na(sell_rally_avg),
               rownum:=order(RSI, decreasing = F),date] 

potential_buys[
  lead1sellrally/lead1open<2 & 
    (((close-low)/(high-low))<.05 ) & 
    ((high/close) > 1.075 |
       ((running_low == low | RSI<.6) & ((avg_range/close) > .05)
       ) 
    )][order(RSI,decreasing=F),head(.SD,5),date]%>%
  with(performance(date,lead1sellrally/lead1open-1,lead1sellrallydate-date,symbol))


# perf drawdown days_traded
# 1: 0.02364706     -5.6         841
# year average drawdown total trades days_traded avg_days_held stocks_traded
# 1: 2006   0.018      0.0   0.0      2           2      4.000000             2
# 2: 2007   0.036      0.0   0.2      7           7      2.714286             3
# 3: 2008   0.037     -1.8   3.5     95          35      5.168421            37
# 4: 2009   0.009     -0.4   0.3     36          17      4.888889            23
# 5: 2010   0.042     -0.2   4.5    106          35      4.358491            22
# 6: 2011   0.025     -2.2   3.9    156          64      4.243590            51
# 7: 2012   0.019     -0.4   0.7     39          18      3.615385            15
# 8: 2013   0.018     -0.3   1.2     67          27      3.865672            21
# 9: 2014   0.058     -0.3   4.0     69          29      4.826087            33
# 10: 2015   0.028     -1.3   4.3    153          67      4.562092            47
# 11: 2016   0.015     -5.0   3.2    214          97      6.191589            44
# 12: 2017  -0.009     -1.4  -0.6     70          44      6.342857            13
# 13: 2018   0.012     -3.9   2.7    225          81      5.653333            40
# 14: 2019   0.026     -2.7   3.2    123          66      4.406504            23
# 15: 2020   0.011     -5.6   2.7    238          86      4.605042           101
# 16: 2021   0.033     -1.3   6.5    194          88      4.716495            54
# 17: 2022   0.024     -2.1   6.3    264          78      4.776515            79         80       87
prices[,rownum:=order(lagging_corr_long, decreasing = F),date] 

prices[volume>250000 & close>7 & !short &
         avg_delta_short<.975 & lagging_corr_long> .35][
           order(rownum),.SD[1:5],date]%>%
  with(performance(date,lead1sellrally/lead1open-1,lead1sellrallydate-date,symbol))
