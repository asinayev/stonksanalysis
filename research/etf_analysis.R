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

prices[,short:=grepl('bear|inverse', name, ignore.case = T) | (grepl('short', name, ignore.case = T) & !grepl('term|duration|matur|long|income', name, ignore.case = T))]
prices[,lever:=grepl('2x|3x|leverag|ultra', name, ignore.case = T)]

setorder(prices, symbol, date)
lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)
rally_avg(prices,200)
performance_features(prices)
#prices=key_etfs(prices,low_corr_thresh=.33)
 
rally(prices,sell_rule=function(dat){dat$close>dat$lag1close+dat$lag1high-dat$lag1low},varname='sell_rally2')
prices[,lead1sell_rally2:= shift(sell_rally2,1,type='lead'),symbol]
prices[,lead1sell_rally2date:= shift(sell_rally2_date,1,type='lead'),symbol]

# prices[symbol %in% prices[,.N,symbol][N>25,symbol]
#        ,mid_range:= zoo::rollapply((high-low)/open, median ,width = 25, align='right',fill=NA),symbol ]
# prices[symbol %in% prices[,.N,symbol][N>25,symbol]
#        ,mid_range:= zoo::rollapply(abs(close-lag1close)/open, median ,width = 25, align='right',fill=NA),symbol ]


tru_rally = prices[symbol%in%c('TNA','UPRO','YINN') &
                 close>lag1close*1.03 ][
                   order(day_drop_norm, decreasing=F),head(.SD,1),date] %>%
  with(performance(date,lead1sell_rally2/lead1open-1,lead1sell_rally2date-date,symbol,lead1sell_rally2date,hold_less_than=1))

# Rally ETFs
#       avg_year   avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.007736842 0.006183746     -1.2           513        1717        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2004   0.012      0.0   0.1     11          12        3      2.818182             7
#  2: 2005   0.008      0.0   0.4     52          53        5      5.096154            19
#  3: 2006   0.002     -0.4   0.1     52          53        5      5.538462            20
#  4: 2007   0.008     -0.1   0.5     62          63        5      5.677419            23
#  5: 2008   0.020     -0.2   1.0     48          49        5      5.479167            23
#  6: 2009   0.026      0.0   1.3     50          51        5      4.320000            20
#  7: 2010   0.014     -0.1   1.0     70          71        5      4.428571            29
#  8: 2011   0.013     -0.1   0.9     73          74        5      4.383562            38
#  9: 2012   0.000     -0.5   0.0     72          73        5      5.694444            33
# 10: 2013   0.007     -0.3   0.7    103         104        5      5.223301            51
# 11: 2014   0.000     -0.2   0.0     99         100        5      5.313131            43
# 12: 2015   0.005     -0.4   0.5    121         122        5      5.586777            48
# 13: 2016   0.005     -0.9   0.7    131         132        5      4.732824            51
# 14: 2017   0.005     -0.2   0.5    104         105        5      7.000000            37
# 15: 2018  -0.001     -0.4  -0.1    148         149        5      5.371622            69
# 16: 2019   0.006     -0.4   0.6    107         108        5      4.476636            50
# 17: 2020   0.006     -1.2   0.8    140         141        5      5.021429            68
# 18: 2021   0.006     -1.2   1.0    167         168        5      5.137725            78
# 19: 2022   0.005     -0.3   0.5     88          89        5      8.375000            51


setorder(prices, symbol, date)
prices[volume>1000000 & close>7 & 
         lead1sell_rally/lead1open<2 & 
         close<lag1high & sell_rally_day>10  ][
           order(day_drop_norm, decreasing=F),head(.SD,1),date] %>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol,lead1sell_rallydate,hold_less_than=5))

# revert ETFs
#      avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.01331579 0.01433371     -1.2           350         912        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2004   0.013      0.0   0.1      5           6        1      2.600000             4
#  2: 2005   0.003      0.0   0.1     22          23        2      6.136364            16
#  3: 2006   0.012     -0.1   0.1     11          12        3      5.454545            11
#  4: 2007   0.010      0.0   0.2     20          21        3      6.650000            18
#  5: 2008   0.008     -0.6   0.4     46          47        5      5.913043            28
#  6: 2009   0.008     -0.4   0.2     28          29        5      6.392857            20
#  7: 2010   0.017     -0.2   0.5     30          31        5      5.866667            21
#  8: 2011   0.019     -0.2   0.8     41          42        5      5.219512            29
#  9: 2012   0.005     -0.1   0.2     40          41        4      6.225000            30
# 10: 2013   0.014     -0.1   0.6     43          44        4      6.093023            31
# 11: 2014   0.017     -0.2   0.9     55          56        5      6.163636            38
# 12: 2015   0.024     -0.3   1.3     55          56        5      5.236364            27
# 13: 2016   0.028     -0.6   1.7     59          60        4      6.186441            32
# 14: 2017   0.007     -0.1   0.2     31          32        5      7.032258            30
# 15: 2018   0.021     -0.2   1.3     60          61        5      5.350000            39
# 16: 2019   0.008     -0.2   0.5     61          62        4      4.573770            39
# 17: 2020   0.010     -1.1   1.0    108         109        5      5.731481            49
# 18: 2021   0.008     -0.3   0.6     79          80        5      5.949367            53
# 19: 2022   0.021     -1.2   2.1     99         100        5      4.898990            55


prices[volume>1000000 & close>7 & (lead1sell_rally/lead1open<2)  & 
         (((close-low)/avg_range)<.15 ) & 
           (((high-close) > avg_range*2) | (avg_delta< ifelse(lever,.98,.99)))
         ][order( day_drop_norm, decreasing=F),head(.SD,1),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol,lead1sell_rallydate,hold_less_than=5))

# Corr long etfs

#      avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.02847059 0.02453066     -1.1           465         816        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2006   0.076      0.0   0.1      1           2        1      2.000000             1
#  2: 2007   0.037      0.0   0.2      5           6        1      2.000000             2
#  3: 2008   0.024     -0.4   0.6     25          26        5      4.520000            15
#  4: 2009   0.035      0.0   0.4     10          11        2      3.300000             6
#  5: 2010   0.042     -0.1   1.6     38          39        5      4.263158            11
#  6: 2011   0.030     -0.3   1.8     61          62        5      4.114754            22
#  7: 2012   0.016     -0.1   0.2     12          13        2      4.083333             9
#  8: 2013   0.017     -0.3   0.4     25          26        5      4.040000             8
#  9: 2014   0.038     -0.2   0.9     24          25        5      6.875000            10
# 10: 2015   0.034     -0.2   1.3     39          40        5      5.128205            15
# 11: 2016   0.058     -0.9   4.5     77          78        5      5.545455            14
# 12: 2017  -0.019     -1.1  -0.5     28          29        5      6.285714             5
# 13: 2018   0.015     -0.8   1.0     70          71        5      5.885714            14
# 14: 2019   0.032     -0.8   2.1     66          67        5      4.893939            15
# 15: 2020   0.026     -1.1   2.4     94          95        5      4.638298            29
# 16: 2021   0.009     -1.1   1.0    114         115        5      6.736842            26
# 17: 2022   0.014     -1.1   1.6    110         111        5      5.363636            34


corr_long = prices[volume>1000000 & close>7 & 
         (avg_delta_short<.975) & lagging_corr_long> .35][
           order(day_drop_norm, decreasing=F),head(.SD,1),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol, lead1sell_rallydate, hold_less_than = 5))

# Drop ETFs

#      avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.01933333 0.01845494     -2.2           240         950        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2005   0.010      0.0   0.0      1           2        1      1.000000             1
#  2: 2006   0.022      0.0   0.2     10          11        4      3.800000             6
#  3: 2007   0.018      0.0   0.3     18          19        4      2.888889             9
#  4: 2008   0.016     -0.7   0.9     58          59        5      4.706897            24
#  5: 2009   0.027     -0.3   1.1     41          42        5      4.878049            14
#  6: 2010   0.037     -0.1   0.7     20          21        4      4.200000             8
#  7: 2011   0.025     -0.2   1.2     48          49        4      3.812500            27
#  8: 2012   0.018     -0.2   0.6     33          34        4      4.303030            10
#  9: 2013   0.023     -0.1   0.9     40          41        4      3.775000            12
# 10: 2014   0.033     -0.1   1.7     52          53        5      5.000000            13
# 11: 2015   0.021     -0.2   1.6     76          77        5      4.776316            22
# 12: 2016   0.023     -1.0   1.8     79          80        5      5.341772            17
# 13: 2017   0.005     -0.3   0.1     26          27        5      4.961538             7
# 14: 2018   0.009     -1.0   0.5     55          56        5      4.400000            16
# 15: 2019   0.026     -0.5   1.8     71          72        5      5.464789            21
# 16: 2020   0.020     -2.2   2.3    115         116        5      4.652174            37
# 17: 2021   0.009     -0.8   1.0    106         107        5      5.358491            30
# 18: 2022   0.006     -1.9   0.5     83          84        5      5.963855            41

drop_etfs = prices[volume>1000000 & close>7 & !short &
                     (avg_delta_short < ifelse(lever,.96,.98) ) ][
           order(day_drop_norm, decreasing=F),head(.SD,1),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol, 
                   lead1sell_rallydate, hold_less_than = 5))

# deviant_etfs
#       avg_year avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.008631579 0.0107027     -1.7           621         944        1
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2004   0.007      0.0   0.1     13          14        1      5.692308             5
#  2: 2005   0.010      0.0   0.5     49          50        1      5.775510            22
#  3: 2006   0.002     -0.2   0.1     46          47        1      6.195652            19
#  4: 2007   0.000     -0.3   0.0     53          54        1      5.264151            23
#  5: 2008   0.006     -0.3   0.3     51          52        1      5.490196            22
#  6: 2009   0.025     -0.1   1.3     53          54        1      5.377358            17
#  7: 2010   0.003     -0.3   0.2     48          49        1      6.125000            16
#  8: 2011   0.016     -0.5   0.8     53          54        1      5.452830            16
#  9: 2012   0.011     -0.2   0.7     61          62        1      4.721311            23
# 10: 2013   0.005     -0.4   0.2     49          50        1      6.061224            19
# 11: 2014   0.021     -0.3   1.2     55          56        1      5.109091            17
# 12: 2015   0.021     -0.4   1.3     60          61        1      4.750000            17
# 13: 2016  -0.003     -0.8  -0.1     48          49        1      6.187500            16
# 14: 2017   0.009     -0.7   0.5     52          53        1      5.423077            16
# 15: 2018   0.026     -0.3   1.4     56          57        1      4.892857            21
# 16: 2019  -0.019     -1.0  -0.7     40          41        1      7.875000            12
# 17: 2020   0.043     -1.0   2.5     58          59        1      4.706897            21
# 18: 2021   0.015     -0.4   0.7     47          48        1      6.425532            16
# 19: 2022  -0.034     -1.7  -1.1     33          34        1      5.757576            10

prices[volume>500000 & close>7 & lead1sell_rally/lead1open<1.5 &
           ((lag1close-close) > avg_range*.25) ][
             order(sd_from0, decreasing=T),head(.SD,1),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol, lead1sell_rallydate, hold_less_than = 1))

# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.01871429 0.01807512     -0.9           489         440        5
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2009   0.033      0.0   0.2      7           8        3      5.000000             5
# 2: 2010   0.031     -0.1   0.7     22          23        4      5.000000             9
# 3: 2011   0.007     -0.5   0.2     30          31        5      4.300000            12
# 4: 2012   0.005     -0.1   0.1     14          15        3      4.214286            10
# 5: 2013   0.014     -0.1   0.2     11          12        2      5.909091             7
# 6: 2014   0.009     -0.1   0.1      9          10        2      3.333333             8
# 7: 2015   0.020     -0.1   0.6     28          29        5      4.464286            12
# 8: 2016   0.023     -0.3   0.3     13          14        2      4.384615             6
# 9: 2017   0.031      0.0   0.5     17          18        3      3.647059            11
# 10: 2018   0.004     -0.2   0.1     19          20        3      6.052632            10
# 11: 2019   0.027     -0.1   0.4     15          16        2      4.933333            12
# 12: 2020   0.015     -0.9   2.3    148         149        5      4.520270            21
# 13: 2021   0.012     -0.3   0.5     45          46        3      4.977778            18
# 14: 2022   0.031     -0.5   1.5     48          49        4      3.791667            15
all_matching_pairs=parallel::mclapply(c(2009:2022),matching_pairs_for_year,
                                      dataset=prices, reference_etfs=reference_etfs,
                                      mc.cores=2)%>%
  rbindlist

arb_etfs = all_matching_pairs[abs(1-close/lag1close-(reference_delta-1)*round(mult.reference_delta_short))>.0075  & avg_delta_short<1 &
                                rsq>.98 & abs(mult.reference_delta_short-round(mult.reference_delta_short))<.15 &
                                volume>1000000][
                                  order(day_drop_norm, decreasing=F),head(.SD,1),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol,
                   lead1sell_rallydate, hold_less_than = 5))


# avg_year   avg_trade drawdown drawdown_days days_traded max_held
# <num>       <num>    <num>         <int>       <int>    <num>
#   1:    0.012 0.008802817     -0.5           464         295        1
# Key: <year>
#   year average drawdown total trades days_traded max_held avg_days_held
# <int>   <num>    <num> <num>  <num>       <int>    <num>         <num>
#   1:  2012   0.000     -0.1   0.0     11          12        1      3.272727
# 2:  2013   0.023     -0.1   0.5     23          24        1      3.608696
# 3:  2014   0.004     -0.2   0.1     20          21        1      5.500000
# 4:  2015   0.018     -0.2   0.6     35          36        1      3.628571
# 5:  2016   0.016     -0.3   0.4     25          26        1      3.840000
# 6:  2017   0.046      0.0   0.2      5           6        1      5.400000
# 7:  2018   0.001     -0.2   0.0     25          26        1      4.640000
# 8:  2019   0.010     -0.1   0.3     26          27        1      3.307692
# 9:  2020  -0.003     -0.5  -0.1     47          48        1      3.936170
# 10:  2021   0.011     -0.2   0.3     29          30        1      3.379310
# 11:  2022   0.006     -0.1   0.2     38          39        1      3.842105
volatility_df = prices[symbol=='SVXY']%>%
  merge(prices[symbol=='UVXY'], by = 'date', suffixes=c('_SVXY','_UVXY'))%>%
  merge(prices[symbol=='SPY'], by = 'date')
volatility_df[(high/low>1.015) |(abs(close/lag1close-1)>.01) | (abs(avg_delta_short-1)>.005)]%>%
  with(performance(date,lead1sell_rally_SVXY/lead1open_SVXY-1,lead1sell_rallydate_SVXY-date,
                   symbol_SVXY,lead1sell_rallydate_SVXY,hold_less_than=1))



helds = merge(rally, revert, on='date',all=T, suffixes = c("rally",'revert') )%>%
  merge(corr_long, on='date', all=T)%>%
  merge(drop_etfs, on='date', all=T, suffixes = c("corr_long",'drop_etfs'))%>%
  merge(arb_etfs, on='date', all=T)
helds[,sum_held:=rowSums(.SD,na.rm=T),.SDcols=c("n_heldrally","n_heldrevert","n_heldcorr_long","n_helddrop_etfs","n_held")]
helds[order(sum_held)]
cor(helds[,.(n_heldrally,n_heldrevert,n_heldcorr_long,n_helddrop_etfs,n_held)],use = 'pairwise.complete')