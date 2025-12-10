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

etf_list=fread("other_datasources/etf_list.csv")

prices=merge(prices, etf_list[,.(ticker, category, leverage)], by.x='symbol', by.y='ticker', all.x = T)

prices[,short:=grepl('bear|inverse', name, ignore.case = T) | (grepl('short', name, ignore.case = T) & !grepl('term|duration|matur|long|income', name, ignore.case = T))]
prices[,lever:=grepl('2x|3x|leverag|betapro', name, ignore.case = T) | (grepl('ultra', name, ignore.case = T) & !grepl('income|muni|bond|govern|investment grade', name, ignore.case = T)) ]

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
                     order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally2/lead1open-1,lead1sell_rally2date-lead1date,symbol,lead1sell_rally2date,hold_max = 1,buy_per_day_max = 1, hold_same_max = F))


# Rally ETFs
#    yr_total_per_held  avg_trade drawdown total_per_drwdn drawdown_days days_traded max_held
# 1:           0.19125 0.01657696     -1.4              11           538         945        5
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
prices[volume>1000000 & close>7 & category %in% c('equity basket', 'physical commodities') &
         lead1sell_rally/lead1open<2 & sd_from0>.015 &
         close<lag1high & sell_rally_day>10   ][
           order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,lead1sell_rallydate,hold_max = 5,buy_per_day_max = 1, hold_same_max = F))

# revert ETFs
#    yr_total_per_held  avg_trade drawdown total_per_drwdn drawdown_days days_traded max_held
# 1:         0.1455556 0.01469933     -1.2              11           579         917        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2004   0.013      0.0   0.1      5           6        1      1.200000             4
#  2: 2005   0.004      0.0   0.1     22          23        2      4.818182            15
#  3: 2006   0.009     -0.1   0.1     11          12        2      3.818182            10
#  4: 2007   0.008     -0.1   0.2     20          21        2      5.650000            19
#  5: 2008   0.006     -0.7   0.3     45          46        5      4.244444            23
#  6: 2009   0.007     -0.5   0.2     24          25        4      5.541667            18
#  7: 2010   0.016     -0.2   0.5     31          32        4      4.225806            22
#  8: 2011   0.016     -0.6   0.7     42          43        4      3.880952            26
#  9: 2012   0.008     -0.1   0.3     39          40        3      4.230769            29
# 10: 2013   0.010     -0.1   0.4     44          45        4      5.772727            30
# 11: 2014   0.013     -0.2   0.7     55          56        5      4.927273            34
# 12: 2015   0.020     -0.3   1.1     57          58        5      4.052632            27
# 13: 2016   0.022     -1.0   1.3     58          59        5      5.275862            32
# 14: 2017   0.018     -0.2   0.6     32          33        5      5.937500            25
# 15: 2018   0.019     -0.3   1.2     62          63        5      4.225806            35
# 16: 2019   0.016     -0.1   1.0     62          63        4      3.209677            36
# 17: 2020   0.010     -1.1   1.1    111         112        5      4.108108            45
# 18: 2021   0.006     -0.3   0.5     78          79        5      4.756410            51
# 19: 2022   0.028     -1.2   2.8    100         101        5      3.840000            51

prices[volume>1000000 & close>7 & (lead1sell_rally/lead1open<2)  & category %in% c('equity basket', 'physical commodities') &
         (((close-low)/avg_range)<.1 ) & 
         sd_from0>.025][
             order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,lead1sell_rallydate,hold_max = 5,buy_per_day_max = 1, hold_same_max = F))

# Corr long etfs

#    yr_total_per_held  avg_trade drawdown total_per_drwdn drawdown_days days_traded max_held
# 1:         0.3386667 0.02357798     -1.9        13.52632           455        1106        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2006   0.076      0.0   0.1      1           2        1      1.000000             1
#  2: 2007   0.023      0.0   0.2      7           8        2      1.000000             2
#  3: 2008   0.041     -0.3   1.5     37          38        5      2.702703            16
#  4: 2009   0.045     -0.3   1.1     25          26        5      3.680000             5
#  5: 2010   0.036     -0.1   1.5     42          43        5      2.880952             9
#  6: 2011   0.008     -1.6   0.5     66          67        5      3.318182            16
#  7: 2012   0.024     -0.1   0.4     16          17        2      2.125000             8
#  8: 2013   0.017     -0.3   0.4     25          26        5      2.520000             7
#  9: 2014   0.040     -0.2   1.4     36          37        5      5.194444            10
# 10: 2015   0.017     -1.2   1.1     63          64        5      3.285714            16
# 11: 2016   0.045     -1.0   4.2     93          93        5      4.440860            10
# 12: 2017  -0.010     -1.0  -0.4     40          41        5      4.500000             7
# 13: 2018   0.018     -0.9   1.5     81          82        5      4.308642            15
# 14: 2019   0.008     -1.5   0.8     99         100        5      5.151515            16
# 15: 2020   0.034     -1.3   5.7    168         169        5      3.464286            34
# 16: 2021   0.025     -0.7   4.0    160         161        5      4.312500            24
# 17: 2022   0.013     -1.9   1.7    131         132        5      4.251908            33

corr_long = prices[volume>1000000 & close>7 & category %in% c('equity basket', 'physical commodities') &
                     avg_delta_short<.975 & lagging_corr_long> .7][
                       order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,lead1sell_rallydate,hold_max = 5,buy_per_day_max = 1, hold_same_max = F))

# Drop ETFs

#    yr_total_per_held  avg_trade drawdown total_per_drwdn drawdown_days days_traded max_held
# 1:         0.2776471 0.02215962     -1.9        12.42105           288        1082        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2005  -0.011      0.0   0.0      1           2        1      7.000000             1
#  2: 2006   0.031      0.0   0.3     10          11        3      2.500000             4
#  3: 2007   0.018      0.0   0.3     18          19        3      1.611111             8
#  4: 2008   0.028     -0.3   1.7     62          63        5      3.129032            23
#  5: 2009   0.032     -0.3   1.2     37          38        5      3.837838             9
#  6: 2010   0.038     -0.1   0.8     20          21        4      3.050000             9
#  7: 2011   0.040     -0.3   2.5     64          65        5      2.125000            27
#  8: 2012   0.017     -0.2   0.5     30          31        3      3.233333            10
#  9: 2013   0.023     -0.1   0.9     40          41        3      2.500000            13
# 10: 2014   0.035     -0.2   1.9     54          55        5      3.314815            14
# 11: 2015   0.019     -1.2   1.8     96          96        5      3.135417            21
# 12: 2016   0.027     -1.4   2.2     82          83        5      4.268293            15
# 13: 2017   0.001     -0.4   0.0     26          27        5      3.269231             6
# 14: 2018   0.015     -1.0   0.8     56          57        5      3.196429            17
# 15: 2019   0.025     -0.5   2.0     79          80        5      4.075949            20
# 16: 2020   0.032     -1.9   4.3    137         138        5      3.160584            42
# 17: 2021   0.019     -0.4   2.5    129         130        5      3.736434            38
# 18: 2022   0.000     -1.8  -0.1    124         125        5      4.274194            31

drop_etfs = prices[volume>1000000 & close>7 & category %in% c('equity basket', 'physical commodities') &
                     avg_delta_short < ifelse(lever,.96,.98)   ][
                       order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,lead1sell_rallydate,hold_max = 5,buy_per_day_max = 1, hold_same_max = F))

# deviant_etfs
#    yr_total_per_held   avg_trade drawdown total_per_drwdn drawdown_days days_traded max_held
# 1:         0.3242105 0.007968952     -2.5           12.32           353        3875        6
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2004   0.009      0.0   0.4     49          50        5      3.489796            12
#  2: 2005   0.007     -0.2   1.6    214         215        5      4.023364            30
#  3: 2006   0.005     -0.4   1.1    216         217        5      4.453704            28
#  4: 2007   0.001     -0.6   0.1    205         206        5      3.482927            39
#  5: 2008   0.013     -0.8   2.8    210         210        5      3.776190            40
#  6: 2009   0.015     -0.6   3.3    218         218        5      3.201835            38
#  7: 2010   0.007     -0.6   1.4    212         212        5      4.410377            27
#  8: 2011   0.008     -1.2   1.8    219         219        5      4.251142            24
#  9: 2012   0.006     -1.3   1.5    229         230        5      3.655022            36
# 10: 2013   0.005     -0.8   1.0    222         223        5      4.184685            38
# 11: 2014   0.011     -0.4   2.6    224         225        5      4.049107            39
# 12: 2015   0.014     -1.2   3.2    229         229        5      3.563319            34
# 13: 2016   0.012     -2.5   2.6    214         214        6      4.406542            31
# 14: 2017   0.006     -1.0   1.2    212         212        5      4.500000            30
# 15: 2018   0.004     -1.5   0.8    217         217        5      4.437788            32
# 16: 2019   0.005     -2.2   1.1    207         208        5      4.811594            20
# 17: 2020   0.011     -2.3   2.5    219         220        5      4.191781            30
# 18: 2021   0.014     -1.0   2.9    211         211        5      4.511848            21
# 19: 2022  -0.008     -2.3  -1.1    138         139        5      4.224638            16

prices[volume>500000 & close>7 &  lead1sell_rally/lead1open<1.5 & category %in% c('equity basket', 'physical commodities') &
         ((lag1close-close) > avg_range*.25)  ][
           order(date,-sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,lead1sell_rallydate,hold_max = 5,buy_per_day_max = 1, hold_same_max = F))

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
                                volume>500000][
                                  order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,lead1sell_rallydate,hold_max = 5,buy_per_day_max = 1, hold_same_max = F))

####################
### corr_reverse
####################
#    yr_total_per_held  avg_trade drawdown total_per_drwdn drawdown_days days_traded max_held
# 1:         0.1623529 0.01369863     -1.9        7.368421           510        1040        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2005   0.043      0.0   0.2      5           6        5     10.600000             1
#  2: 2006   0.009      0.0   0.1     16          17        2      1.000000             2
#  3: 2007   0.025      0.0   0.4     16          17        2      0.625000             6
#  4: 2008   0.013     -0.4   0.8     59          60        5      2.932203            24
#  5: 2009   0.025     -0.1   0.7     29          30        5      2.758621             6
#  6: 2010   0.002     -0.2   0.1     26          27        5      4.500000             9
#  7: 2011   0.021     -0.3   0.8     38          39        5      3.078947            14
#  8: 2012   0.011     -0.1   0.5     45          46        4      2.177778             7
#  9: 2013   0.013     -0.1   0.5     36          37        4      2.166667            12
# 10: 2014   0.017     -0.4   1.2     73          74        5      3.465753            21
# 11: 2015   0.007     -0.5   0.6     96          97        5      3.197917            18
# 12: 2016   0.028     -0.2   1.7     60          61        5      3.033333            11
# 13: 2017   0.004     -0.9   0.2     56          57        5      3.857143            10
# 14: 2018  -0.017     -1.2  -1.1     65          66        5      4.553846            20
# 15: 2019   0.026     -1.2   2.4     95          96        5      2.094737            20
# 16: 2020   0.021     -1.9   2.8    132         133        5      3.674242            32
# 17: 2021   0.021     -0.2   1.8     85          86        5      3.694118            17
# 18: 2022   0.003     -1.8   0.3     90          91        5      3.533333            33

corr_reverse = prices[volume>1000000 & close>7 & !short & category %in% c('equity basket', 'physical commodities') &
           (avg_root_delta< -.02) & root_delta_corr > .15][
             order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,lead1sell_rallydate,hold_max = 5,buy_per_day_max = 1, hold_same_max = F))



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
