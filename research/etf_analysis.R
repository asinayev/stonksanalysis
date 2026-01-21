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

prices=lapply(2004:2025,
              function(yr){
                x=fread(paste0("/home/rstudio/datasets/stocks_by_yr/",yr,".csv.gz"), colClasses = c(cik = "character"))
                if(nrow(x)<1000){
                  x=data.table(read.csv(paste0("/home/rstudio/datasets/stocks_by_yr/",yr,".csv.gz"), colClasses = c(cik = "character")))
                  x[,date := as.IDate(date)]
                  x[,list_date := as.IDate(list_date)]
                }
                subset(x, type %in% c('ETF','INDEX','ETV'))
              })

prices = rbindlist(prices, use.names=T, fill=T)
setnames(prices, 'stock', 'symbol')
prices = prices[ volume*open*close > 0]

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
#    yr_total_per_held avg_trade drawdown total_per_drwdn drawdown_days days_traded max_held
# 1:         0.1652632 0.0147032     -1.8        8.944444           615        1116        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2004   0.028      0.0   0.1      4           5        1      0.500000             2
#  2: 2005   0.025      0.0   0.2      6           7        3      2.666667             3
#  3: 2006   0.011      0.0   0.1      5           5        2      0.600000             3
#  4: 2007   0.008     -0.1   0.2     29          30        5      3.827586             9
#  5: 2008   0.008     -0.8   0.5     65          66        5      5.646154            31
#  6: 2009   0.061     -0.2   0.8     13          14        5      3.692308             3
#  7: 2010   0.021     -0.2   0.8     39          40        5      2.717949            12
#  8: 2011   0.032     -0.1   1.4     43          44        5      3.697674            17
#  9: 2012  -0.010     -0.6  -0.3     25          26        5      7.440000            11
# 10: 2013   0.029     -0.3   1.0     36          37        5      3.222222            11
# 11: 2014   0.017     -0.2   0.8     49          50        5      2.979592            15
# 12: 2015   0.025     -0.1   1.2     46          47        5      2.630435            16
# 13: 2016   0.013     -0.3   0.8     59          60        5      3.745763            21
# 14: 2017   0.013     -0.2   0.5     37          38        5      2.945946            10
# 15: 2018   0.003     -0.5   0.2     68          69        5      2.750000            26
# 16: 2019   0.005     -0.3   0.2     30          31        5      3.233333            10
# 17: 2020   0.002     -1.8   0.2     76          77        5      3.236842            28
# 18: 2021   0.005     -0.4   0.5     92          93        5      3.619565            34
# 19: 2022   0.032     -0.8   3.7    117         118        5      2.760684            40
# 20: 2023   0.006     -0.9   0.8    121         122        5      3.123967            38
# 21: 2024   0.016     -0.9   1.2     74          75        5      4.135135            21
# 22: 2025   0.019     -0.5   1.2     61          62        5      2.868852            24


setorder(prices, symbol, date)
prices[(volume*close/unadjClose) >1000000 & unadjClose>7 & category %in% c('equity basket') &
         lead1sell_rally/lead1open<2 & sd_from0>.015 &
         close<lag1high & sell_rally_day>10   ][
           order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,lead1sell_rallydate,
                   hold_max = 5,buy_per_day_max = 1, hold_same_max = F))

# revert ETFs
#    yr_total_per_held  avg_trade drawdown total_per_drwdn drawdown_days days_traded max_held
# 1:         0.2018182 0.01767516     -1.2            18.5           652        1278        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2004  -0.004     -0.3  -0.1     25          26        5      3.440000            11
#  2: 2005   0.002     -0.2   0.1     35          36        3      4.885714            17
#  3: 2006   0.016     -0.2   0.4     22          23        4      3.727273            13
#  4: 2007   0.010     -0.1   0.3     35          36        3      3.800000            22
#  5: 2008   0.021     -0.9   2.0     96          97        5      4.020833            37
#  6: 2009   0.031     -0.1   0.7     21          22        4      3.380952             5
#  7: 2010   0.021     -0.2   0.7     35          36        4      4.028571            21
#  8: 2011   0.024     -0.8   1.6     67          68        4      3.791045            25
#  9: 2012   0.011     -0.1   0.3     26          27        3      4.384615            19
# 10: 2013   0.022     -0.1   0.9     39          40        4      5.025641            21
# 11: 2014   0.027     -0.1   1.4     52          53        5      3.692308            24
# 12: 2015   0.032     -0.5   1.6     51          52        4      3.156863            19
# 13: 2016   0.042     -0.7   2.5     61          62        5      4.442623            26
# 14: 2017   0.020     -0.1   0.4     21          22        3      4.380952            17
# 15: 2018   0.007     -0.5   0.3     41          42        5      5.390244            25
# 16: 2019   0.025     -0.4   0.9     37          38        5      3.000000            24
# 17: 2020   0.005     -1.2   0.5     91          92        5      4.164835            32
# 18: 2021   0.020     -0.2   1.4     73          74        4      3.945205            35
# 19: 2022   0.028     -0.8   4.8    168         169        5      3.946429            39
# 20: 2023   0.010     -1.1   1.1    107         108        5      4.411215            34
# 21: 2024  -0.003     -1.2  -0.3     76          77        5      6.065789            27
# 22: 2025   0.009     -0.6   0.7     77          78        4      4.519481            37

prices[(volume*close/unadjClose) >1000000 & unadjClose>7 & (lead1sell_rally/lead1open<2)  & category %in% c('equity basket','physical commodities') &
         (((close-low)/avg_range)<.15 ) & 
         (((high-close) > avg_range*2) | (avg_delta< ifelse(lever,.98,.99))| (close/max_price_short< ifelse(lever,.8,.9)) )][
             order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,lead1sell_rallydate,
                   hold_max = 5,buy_per_day_max = 1, hold_same_max = F))

# Corr long etfs

#    yr_total_per_held  avg_trade drawdown total_per_drwdn drawdown_days days_traded max_held
# 1:         0.2284211 0.01610044     -3.6        6.055556           686        1375        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2004  -0.021      0.0   0.0      1           2        1      5.000000             1
#  2: 2006   0.019     -0.1   0.1      6           7        3      2.666667             3
#  3: 2007   0.022      0.0   0.3     12          13        2      1.416667             4
#  4: 2008   0.003     -1.9   0.2     75          76        5      4.013333            18
#  5: 2009   0.064     -0.8   1.5     23          24        5      3.478261             3
#  6: 2010   0.035     -0.2   1.1     33          34        5      3.393939             8
#  7: 2011   0.007     -1.6   0.5     73          74        5      3.561644            14
#  8: 2012   0.013     -0.1   0.2     12          13        1      2.333333             6
#  9: 2013   0.009     -0.3   0.2     23          24        5      2.869565             7
# 10: 2014   0.027     -0.3   0.7     26          27        5      6.115385             8
# 11: 2015   0.020     -1.0   1.0     53          54        5      4.018868            14
# 12: 2016   0.048     -2.2   2.8     59          60        5      4.644068             9
# 13: 2017  -0.038     -1.2  -1.0     27          28        5      5.888889             5
# 14: 2018   0.004     -1.4   0.2     57          58        5      4.456140             9
# 15: 2019   0.034     -0.9   1.7     51          52        5      2.549020             8
# 16: 2020  -0.008     -3.6  -0.8    107         108        5      3.121495            27
# 17: 2021   0.033     -1.0   3.5    106         107        5      3.037736            21
# 18: 2022   0.017     -2.2   3.0    178         179        5      3.713483            34
# 19: 2023   0.019     -0.6   3.0    157         158        5      3.726115            21
# 20: 2024   0.015     -0.8   2.0    130         131        5      4.261538            14
# 21: 2025   0.011     -0.6   1.6    145         146        5      3.882759            20

corr_long = prices[(volume*close/unadjClose) >1000000 & unadjClose>7 & (lead1sell_rally/lead1open<2)  & category %in% c('equity basket','physical commodities') &
                     avg_delta_short<.975 & lagging_corr_long> .7][
                       order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,lead1sell_rallydate,
                   hold_max = 5,buy_per_day_max = 1, hold_same_max = F))

# Drop ETFs

#    yr_total_per_held avg_trade drawdown total_per_drwdn drawdown_days days_traded max_held
# 1:             0.203 0.0165316     -3.7        5.513514           705        1256        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2004   0.015     -0.1   0.1      9          10        2      3.888889             4
#  2: 2005   0.003     -0.1   0.0      6           7        3      2.500000             3
#  3: 2006   0.002     -0.3   0.0     15          16        5      3.800000             8
#  4: 2007   0.018     -0.1   0.5     28          29        3      2.357143             9
#  5: 2008   0.014     -1.1   1.5    104         105        5      3.634615            34
#  6: 2009   0.038     -0.3   1.2     32          33        5      3.656250             3
#  7: 2010   0.037     -0.1   1.1     31          32        5      3.483871            10
#  8: 2011   0.031     -0.3   2.0     63          64        5      3.047619            23
#  9: 2012   0.020     -0.3   0.4     19          20        3      2.789474             9
# 10: 2013   0.017     -0.1   0.6     35          36        4      2.542857            14
# 11: 2014   0.033     -0.1   1.2     36          37        5      3.694444            11
# 12: 2015   0.019     -0.6   1.2     64          65        5      3.515625            19
# 13: 2016   0.025     -1.1   1.4     58          59        5      4.258621            19
# 14: 2017  -0.018     -0.5  -0.3     18          19        5      4.222222             5
# 15: 2018   0.015     -0.5   0.6     42          43        5      3.714286            13
# 16: 2019   0.039     -0.5   1.2     31          32        5      2.709677            12
# 17: 2020  -0.022     -3.7  -2.0     91          92        5      3.802198            28
# 18: 2021   0.020     -2.1   2.0     97          98        5      3.247423            31
# 19: 2022   0.015     -1.9   2.7    181         182        5      3.464088            41
# 20: 2023   0.010     -0.8   1.0    102         103        5      3.627451            23
# 21: 2024   0.028     -0.3   2.4     88          89        5      3.943182            19
# 22: 2025   0.019     -0.7   1.6     84          85        5      3.380952            21

#crypto might be ok
drop_etfs = prices[(volume*close/unadjClose) >1000000 & unadjClose>7 & (lead1sell_rally/lead1open<2) & category %in% c('equity basket', 'physical commodities') &
                     avg_delta_short < ifelse(lever,.96,.98)   ][
                       order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,lead1sell_rallydate,
                   hold_max = 5,buy_per_day_max = 1, hold_same_max = F))

# deviant_etfs
#    yr_total_per_held avg_trade drawdown total_per_drwdn drawdown_days days_traded max_held
# 1:         0.4090909   0.00625     -2.1        4.285714           748        1462        1
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2004  -0.002     -0.4  -0.1     60          61        1      3.650000            23
#  2: 2005   0.007     -0.2   0.5     66          67        1      3.803030            26
#  3: 2006   0.007     -0.2   0.4     64          65        1      4.421875            23
#  4: 2007  -0.001     -0.3  -0.1     65          66        1      3.800000            25
#  5: 2008   0.008     -0.4   0.5     61          62        1      4.393443            26
#  6: 2009   0.019     -0.2   1.2     61          62        1      3.245902             6
#  7: 2010   0.008     -0.2   0.5     62          63        1      4.290323            18
#  8: 2011   0.015     -0.4   1.0     69          70        1      3.971014            14
#  9: 2012   0.008     -0.4   0.5     60          61        1      4.516667            25
# 10: 2013   0.000     -0.6   0.0     62          63        1      4.451613            26
# 11: 2014   0.003     -0.4   0.2     70          71        1      3.957143            29
# 12: 2015   0.002     -0.6   0.1     64          65        1      4.484375            28
# 13: 2016   0.016     -0.6   1.1     68          69        1      3.735294            24
# 14: 2017  -0.001     -0.6   0.0     64          65        1      4.250000            23
# 15: 2018   0.001     -0.5   0.1     75          76        1      3.440000            29
# 16: 2019  -0.002     -0.6  -0.1     71          72        1      3.746479            24
# 17: 2020  -0.003     -2.1  -0.2     65          66        1      4.323077            23
# 18: 2021   0.015     -0.8   1.1     72          73        1      3.541667            19
# 19: 2022   0.006     -0.9   0.4     71          72        1      3.901408            14
# 20: 2023   0.029     -0.4   2.0     71          72        1      3.507042            12
# 21: 2024  -0.002     -0.5  -0.1     60          61        1      4.550000            10
# 22: 2025   0.000     -1.0   0.0     59          60        1      4.745763            14

prices[(volume*close/unadjClose) >500000 & unadjClose>7 & lead1sell_rally/lead1open<1.5 & category %in% c('equity basket') &
         ((lag1close-close) > avg_range*.25)  ][
           order(date,-sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,lead1sell_rallydate,hold_max = 1,buy_per_day_max = 1, hold_same_max = F))

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
#    yr_total_per_held   avg_trade drawdown total_per_drwdn drawdown_days days_traded max_held
# 1:              0.11 0.007980846     -3.5        2.857143           937        1273        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2004   0.010      0.0   0.0      2           2        0      0.000000             2
#  2: 2005  -0.004      0.0   0.0      6           7        4      3.666667             3
#  3: 2006   0.007      0.0   0.1     20          21        4      1.350000             3
#  4: 2007   0.028      0.0   0.6     20          21        2      0.650000             9
#  5: 2008   0.008     -1.1   0.6     77          78        5      3.493506            40
#  6: 2010   0.013     -0.1   0.1      7           8        2      2.000000             2
#  7: 2011  -0.006     -1.3  -0.3     51          52        5      3.215686            15
#  8: 2012   0.009     -0.7   0.2     28          29        4      2.321429             7
#  9: 2013   0.017     -0.5   0.6     36          37        4      2.055556            12
# 10: 2014   0.017     -0.4   1.2     72          73        5      3.569444            18
# 11: 2015   0.009     -0.8   0.7     83          84        5      3.337349            17
# 12: 2016   0.022     -0.3   0.9     42          43        5      3.833333            10
# 13: 2017   0.003     -0.9   0.2     54          55        5      3.851852             9
# 14: 2018  -0.008     -1.0  -0.5     59          60        5      4.186441            21
# 15: 2019   0.025     -0.5   2.1     83          84        5      2.000000            18
# 16: 2020  -0.016     -3.5  -1.7    106         107        5      3.971698            30
# 17: 2021   0.023     -2.0   1.9     81          82        5      3.308642            18
# 18: 2022  -0.002     -2.1  -0.2    136         137        5      3.492647            38
# 19: 2023   0.003     -1.4   0.3    117         118        5      3.683761            16
# 20: 2024   0.011     -0.5   1.2    109         110        5      2.486239            26
# 21: 2025   0.031     -0.4   2.0     64          65        4      2.296875            23

corr_reverse = prices[(volume*close/unadjClose) >1000000 & unadjClose>7  & 
                        !short & category %in% c('equity basket', 'physical commodities') &
           (avg_root_delta< -.03) & root_delta_corr > .15][
             order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,lead1sell_rallydate,
                   hold_max = 3,buy_per_day_max = 1, hold_same_max = F))



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
