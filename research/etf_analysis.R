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
                   order(day_drop_norm/sd_from0, decreasing=F),head(.SD,1),date] %>%
  with(performance(lead1date,lead1sell_rally2/lead1open-1,lead1sell_rally2date-lead1date,symbol,lead1sell_rally2date,hold_less_than=1))

# Rally ETFs
#      avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.01642105 0.01657696     -1.4           538         945        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2004   0.002      0.0   0.0      2           2        1      0.500000             1
#  2: 2005   0.012      0.0   0.0      2           3        1      2.000000             2
#  3: 2006   0.014      0.0   0.1      4           4        2      0.750000             2
#  4: 2007   0.010     -0.1   0.2     20          21        5      4.050000             8
#  5: 2008   0.031     -0.1   1.1     35          36        5      4.057143            18
#  6: 2009   0.044     -0.1   1.6     36          37        5      3.222222            17
#  7: 2010   0.021     -0.1   1.1     51          52        5      2.823529            18
#  8: 2011   0.025     -0.1   1.0     41          42        5      3.487805            17
#  9: 2012  -0.005     -0.5  -0.1     26          27        5      7.153846            12
# 10: 2013   0.030     -0.2   1.2     40          41        5      3.125000            11
# 11: 2014   0.020     -0.2   1.1     55          56        5      2.745455            19
# 12: 2015   0.020     -0.4   1.2     61          62        5      3.655738            19
# 13: 2016  -0.002     -1.4  -0.1     58          58        5      4.103448            20
# 14: 2017   0.019     -0.3   0.7     40          41        5      2.900000            12
# 15: 2018   0.014     -0.5   1.0     72          73        5      2.902778            28
# 16: 2019   0.009     -0.6   0.6     61          62        5      3.786885            16
# 17: 2020   0.012     -1.2   1.2     96          97        5      3.572917            35
# 18: 2021   0.004     -1.0   0.6    138         139        5      3.797101            38
# 19: 2022   0.032     -0.7   2.9     91          92        5      3.439560            34


setorder(prices, symbol, date)
prices[volume>1000000 & close>7 &
         lead1sell_rally/lead1open<2 & sd_from0>.015 &
         close<lag1high & sell_rally_day>10  ][
           order(day_drop_norm/sd_from0, decreasing=F),head(.SD,1),date] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,lead1sell_rallydate,hold_less_than=5))

# revert ETFs
# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.01326316 0.01428571     -1.2           350         922        5
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2004   0.013      0.0   0.1      5           6        1      1.200000             4
# 2: 2005   0.003      0.0   0.1     22          23        2      4.727273            16
# 3: 2006   0.012     -0.1   0.1     11          12        2      3.909091            11
# 4: 2007   0.010      0.0   0.2     20          21        2      5.100000            18
# 5: 2008   0.009     -0.6   0.4     45          46        5      4.555556            27
# 6: 2009   0.007     -0.4   0.2     24          25        4      5.625000            18
# 7: 2010   0.019     -0.2   0.6     31          32        4      4.225806            21
# 8: 2011   0.020     -0.2   0.8     42          43        4      3.880952            30
# 9: 2012   0.006     -0.1   0.2     39          40        3      4.102564            29
# 10: 2013   0.013     -0.1   0.6     44          45        4      5.227273            32
# 11: 2014   0.017     -0.2   0.9     55          56        5      4.763636            38
# 12: 2015   0.025     -0.3   1.4     57          58        5      3.789474            28
# 13: 2016   0.027     -0.6   1.6     59          60        4      4.864407            32
# 14: 2017   0.009     -0.1   0.3     32          33        5      6.031250            30
# 15: 2018   0.015     -0.2   1.0     62          63        5      4.209677            40
# 16: 2019   0.008     -0.2   0.5     62          63        4      3.080645            40
# 17: 2020   0.012     -1.1   1.3    111         112        5      4.144144            49
# 18: 2021   0.007     -0.3   0.5     80          81        5      4.750000            55
# 19: 2022   0.020     -1.2   2.1    102         103        5      3.421569            55

prices[volume>1000000 & close>7 & (lead1sell_rally/lead1open<2)  & 
         (((close-low)/avg_range)<.15 ) & 
           (((high-close) > avg_range*2) | (avg_delta< ifelse(lever,.98,.99)))
         ][order( day_drop_norm/sd_from0, decreasing=F),head(.SD,1),date]%>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,lead1sell_rallydate,hold_less_than=5))

# Corr long etfs

# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.029 0.024     -1.5           463         851        5
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2006   0.076      0.0   0.1      1           2        1      1.000000             1
# 2: 2007   0.037      0.0   0.2      5           6        1      0.400000             2
# 3: 2008   0.026     -0.4   0.7     26          27        5      3.153846            15
# 4: 2009   0.039      0.0   0.3      9          10        1      2.000000             5
# 5: 2010   0.042     -0.1   1.7     40          41        5      2.900000            11
# 6: 2011   0.027     -0.4   1.6     61          62        4      2.688525            21
# 7: 2012   0.016     -0.1   0.2     12          13        2      2.583333             9
# 8: 2013   0.017     -0.3   0.4     25          26        5      2.560000             8
# 9: 2014   0.038     -0.2   0.9     24          25        5      5.375000            10
# 10: 2015   0.031     -0.2   1.3     41          42        5      3.853659            17
# 11: 2016   0.045     -1.2   3.7     82          83        5      4.256098            14
# 12: 2017  -0.019     -1.1  -0.5     28          29        5      5.000000             5
# 13: 2018   0.012     -1.0   0.9     74          75        5      4.391892            14
# 14: 2019   0.031     -0.9   2.2     71          72        5      3.338028            15
# 15: 2020   0.027     -1.1   2.7     99         100        5      3.080808            31
# 16: 2021   0.009     -1.0   1.1    120         121        5      5.233333            27
# 17: 2022   0.010     -1.5   1.2    116         117        5      4.000000            34

corr_long = prices[volume>1000000 & close>7 & 
                     (avg_delta_short<.975) & lagging_corr_long> .75][
                       order(day_drop_norm/sd_from0, decreasing=F),head(.SD,1),date]%>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol, lead1sell_rallydate, hold_less_than = 5))

# Drop ETFs

# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.01911111 0.01752904     -2.2           357         964        5
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2005   0.010      0.0   0.0      1           1        0      0.000000             1
# 2: 2006   0.022      0.0   0.2     10          11        3      2.400000             6
# 3: 2007   0.018      0.0   0.3     18          19        3      1.611111             9
# 4: 2008   0.019     -0.6   1.2     61          62        5      3.278689            26
# 5: 2009   0.029     -0.3   1.1     38          39        5      3.684211            11
# 6: 2010   0.037     -0.1   0.7     20          21        4      2.750000             8
# 7: 2011   0.026     -0.2   1.2     45          46        3      2.111111            27
# 8: 2012   0.017     -0.2   0.5     28          29        3      2.821429             9
# 9: 2013   0.023     -0.1   0.9     40          41        3      2.375000            12
# 10: 2014   0.034     -0.1   1.8     54          55        5      3.388889            14
# 11: 2015   0.021     -0.2   1.7     80          81        5      3.275000            22
# 12: 2016   0.025     -1.0   2.0     82          83        5      3.987805            17
# 13: 2017   0.005     -0.3   0.1     26          27        5      3.461538             7
# 14: 2018   0.009     -1.0   0.5     55          56        5      3.018182            16
# 15: 2019   0.026     -0.5   1.9     72          73        5      4.041667            21
# 16: 2020   0.011     -2.2   1.3    119         120        5      3.563025            38
# 17: 2021   0.006     -1.1   0.7    106         107        5      4.264151            31
# 18: 2022   0.006     -2.0   0.5     92          93        5      4.347826            44

drop_etfs = prices[volume>1000000 & close>7 & !short &
                     (avg_delta_short < ifelse(lever,.96,.98) ) ][
           order(day_drop_norm/sd_from0, decreasing=F),head(.SD,1),date]%>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol, 
                   lead1sell_rallydate, hold_less_than = 5))

# deviant_etfs
#       avg_year   avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.008052632 0.008098353     -3.9           319        4434       12
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2004   0.009      0.0   0.5     51          52        6      3.392157            12
#  2: 2005   0.008     -0.2   1.9    238         239       11      4.058824            30
#  3: 2006   0.006     -0.4   1.3    241         242        8      4.385892            29
#  4: 2007   0.001     -0.6   0.3    234         235       10      3.414530            41
#  5: 2008   0.017     -0.9   4.1    245         245       11      3.808163            41
#  6: 2009   0.016     -0.4   3.7    237         237        7      3.189873            38
#  7: 2010   0.009     -0.7   2.2    252         251       11      4.452381            31
#  8: 2011   0.008     -1.2   1.9    252         251       12      4.246032            27
#  9: 2012   0.004     -1.5   1.0    250         250       12      3.768000            36
# 10: 2013   0.005     -0.9   1.3    252         252        9      4.166667            39
# 11: 2014   0.011     -0.4   2.7    252         252       10      4.007937            39
# 12: 2015   0.015     -1.2   3.7    252         252        9      3.539683            35
# 13: 2016   0.005     -3.9   1.2    252         252       12      4.567460            34
# 14: 2017   0.005     -1.6   1.2    251         251       11      4.533865            32
# 15: 2018   0.007     -1.4   1.8    251         251        8      4.298805            33
# 16: 2019   0.009     -2.1   2.2    252         252       11      4.658730            21
# 17: 2020   0.011     -1.9   2.7    253         253        9      4.268775            31
# 18: 2021   0.011     -0.9   2.9    252         251        8      4.420635            22
# 19: 2022  -0.004     -2.3  -0.7    166         166       10      4.138554            16

prices[volume>500000 & close>7 &  lead1sell_rally/lead1open<1.5 &
         ((lag1close-close) > avg_range*.25)  ][
             order(sd_from0, decreasing=T),head(.SD,1),date]%>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol, 
                   lead1sell_rallydate, hold_less_than = 5))

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
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,
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
  with(performance(lead1date,lead1sell_rally_SVXY/lead1open_SVXY-1,lead1sell_rallydate_SVXY-lead1date,
                   symbol_SVXY,lead1sell_rallydate_SVXY,hold_less_than=1))



helds = merge(rally, revert, on='date',all=T, suffixes = c("rally",'revert') )%>%
  merge(corr_long, on='date', all=T)%>%
  merge(drop_etfs, on='date', all=T, suffixes = c("corr_long",'drop_etfs'))%>%
  merge(arb_etfs, on='date', all=T)
helds[,sum_held:=rowSums(.SD,na.rm=T),.SDcols=c("n_heldrally","n_heldrevert","n_heldcorr_long","n_helddrop_etfs","n_held")]
helds[order(sum_held)]
cor(helds[,.(n_heldrally,n_heldrevert,n_heldcorr_long,n_helddrop_etfs,n_held)],use = 'pairwise.complete')