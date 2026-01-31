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

rbindlist(prices, use.names=T, fill=T)[,.N,year(date)] %>% plot

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
#    yr_total_per_held  avg_trade drawdown total_per_drwdn drawdown_days days_traded max_held
# 1:         0.1589474 0.01092319     -1.4        11.07143           838        1439        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2004   0.028      0.0   0.1      4           5        1      0.500000             2
#  2: 2005   0.024      0.0   0.2      7           7        3      2.285714             3
#  3: 2006   0.014      0.0   0.1      4           4        2      0.750000             2
#  4: 2007   0.013     -0.1   0.3     27          28        5      3.296296            10
#  5: 2008   0.006     -0.9   0.5     73          74        5      5.438356            29
#  6: 2009   0.020     -0.5   1.5     76          77        5      4.013158            29
#  7: 2010   0.008     -0.5   0.6     76          77        5      5.039474            28
#  8: 2011   0.023     -0.4   1.5     66          67        5      3.075758            22
#  9: 2012  -0.012     -1.2  -0.5     38          39        5      7.894737            12
# 10: 2013   0.006     -0.8   0.4     58          59        5      5.310345            16
# 11: 2014   0.003     -0.4   0.2     74          75        5      5.094595            19
# 12: 2015   0.029     -0.2   1.8     61          62        5      2.836066            22
# 13: 2016   0.005     -0.7   0.4     81          82        5      4.259259            26
# 14: 2017  -0.005     -1.0  -0.3     52          53        5      4.711538            10
# 15: 2018   0.018     -0.8   1.7     91          92        5      2.516484            28
# 16: 2019  -0.001     -0.4  -0.1     72          73        5      4.291667            17
# 17: 2020   0.000     -1.4   0.0     86          87        5      5.500000            27
# 18: 2021   0.007     -1.2   0.8    110         111        5      3.581818            37
# 19: 2022   0.024     -0.7   2.4    100         101        5      2.990000            38
# 20: 2023   0.010     -0.6   1.1    108         109        5      3.222222            28
# 21: 2024   0.016     -0.5   1.5     96          97        5      4.218750            27
# 22: 2025   0.022     -0.6   1.3     59          60        5      2.898305            29


setorder(prices, symbol, date)
rally = prices[(volume*close/unadjClose) >1000000 & unadjClose>7 & category %in% c('equity basket') &
         lead1sell_rally/lead1open<2 & sd_from0>.015 &
         close<lag1high & sell_rally_day>10   ][
           order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,lead1sell_rallydate,
                   hold_max = 5,buy_per_day_max = 1, hold_same_max = F))

# revert ETFs
#    yr_total_per_held  avg_trade drawdown total_per_drwdn drawdown_days days_traded max_held
# 1:             0.145 0.01382576     -3.2          4.5625           609        1078        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2004   0.028      0.0   0.1      3           4        1      3.333333             3
#  2: 2005   0.021      0.0   0.0      2           3        1      2.500000             2
#  3: 2006   0.046      0.0   0.5     10          11        3      2.100000             7
#  4: 2007   0.024     -0.1   0.6     24          25        4      2.833333            10
#  5: 2008   0.005     -1.5   0.4     89          90        5      4.808989            37
#  6: 2009   0.014     -1.0   0.7     50          51        4      5.560000            24
#  7: 2010   0.019     -0.2   0.6     31          32        3      4.903226            18
#  8: 2011   0.031     -0.1   1.5     48          49        4      3.979167            29
#  9: 2012   0.013     -0.2   0.6     42          43        3      4.500000            18
# 10: 2013   0.016     -0.4   0.7     44          45        4      3.090909            14
# 11: 2014  -0.002     -1.1  -0.1     55          56        5      5.854545            17
# 12: 2015   0.025     -0.6   1.9     77          78        5      5.129870            28
# 13: 2016   0.015     -1.1   0.8     49          50        5      5.102041            31
# 14: 2017   0.037     -0.1   0.9     24          25        2      3.583333            15
# 15: 2018   0.007     -0.8   0.4     63          64        5      4.857143            29
# 16: 2019   0.028     -0.9   1.3     47          48        4      3.723404            23
# 17: 2020  -0.017     -3.2  -1.0     58          59        5      4.931034            35
# 18: 2021   0.025     -1.4   1.8     73          74        5      4.561644            35
# 19: 2022  -0.005     -2.3  -0.5     97          98        5      4.701031            51
# 20: 2023  -0.002     -1.7  -0.1     65          66        5      5.661538            34
# 21: 2024   0.030     -1.6   1.5     49          50        5      4.836735            31
# 22: 2025   0.036     -0.5   2.0     56          57        4      4.160714            29

revert = prices[(volume*close/unadjClose) >1000000 & unadjClose>7 & (lead1sell_rally/lead1open<2)  & category %in% c('equity basket','physical commodities') &
                  ( ((high-close)/avg_range > 1)  & 
                      (avg_delta< (1-.005*abs(leverage)) ))][
             order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,lead1sell_rallydate,
                   hold_max = 5,buy_per_day_max = 1, hold_same_max = F))

# Corr long etfs

#    yr_total_per_held  avg_trade drawdown total_per_drwdn drawdown_days days_traded max_held
# 1:         0.3263158 0.01262688     -4.6         6.76087          1494        2484        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2004  -0.021      0.0   0.0      1           2        1      5.000000             1
#  2: 2006   0.019     -0.1   0.1      6           7        3      2.666667             3
#  3: 2007   0.020      0.0   0.3     13          14        2      2.384615             5
#  4: 2008   0.004     -1.5   0.5    116         117        5      3.646552            30
#  5: 2009   0.024     -0.9   2.8    120         121        5      4.191667            23
#  6: 2010   0.040     -0.9   3.2     79          80        5      3.291139            20
#  7: 2011   0.007     -1.6   0.8    109         110        5      3.926606            24
#  8: 2012   0.013     -0.4   1.1     84          85        5      4.345238            14
#  9: 2013   0.008     -2.1   0.7     94          95        5      4.265957             9
# 10: 2014   0.018     -2.4   1.9    107         108        5      4.897196            15
# 11: 2015   0.022     -1.1   3.3    151         152        5      3.311258            14
# 12: 2016  -0.013     -3.5  -1.9    152         153        5      5.223684            21
# 13: 2017  -0.006     -4.6  -0.7    109         110        5      4.596330            14
# 14: 2018  -0.003     -4.1  -0.4    137         138        5      4.437956            22
# 15: 2019   0.023     -4.4   4.0    172         173        5      3.447674            23
# 16: 2020   0.011     -3.3   2.0    181         182        5      4.209945            39
# 17: 2021   0.023     -1.3   4.0    175         176        5      3.520000            23
# 18: 2022   0.011     -1.9   2.0    193         194        5      3.652850            36
# 19: 2023   0.011     -1.0   1.6    151         152        5      4.483444            27
# 20: 2024   0.019     -0.8   2.9    152         153        5      3.888158            24
# 21: 2025   0.018     -0.9   2.9    161         162        5      3.322981            27

corr_long = prices[(volume*close/unadjClose) >1000000 & unadjClose>7 & (lead1sell_rally/lead1open<2)  & category %in% c('equity basket','physical commodities') &
                     avg_delta_short<.975 & lagging_corr_long> .7][
                       order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,lead1sell_rallydate,
                   hold_max = 5,buy_per_day_max = 1, hold_same_max = F))

# Drop ETFs

#    yr_total_per_held  avg_trade drawdown total_per_drwdn drawdown_days days_traded max_held
# 1:         0.4111111 0.01519507     -1.4        5.285714           817         508        1
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2005  -0.005      0.0   0.0      1           2        1      8.000000             1
#  2: 2006   0.047      0.0   0.0      1           2        1      1.000000             1
#  3: 2007  -0.003      0.0   0.0      2           3        1      4.000000             2
#  4: 2008   0.021     -0.5   0.5     25          26        1      4.160000            13
#  5: 2009   0.025     -0.5   0.4     17          18        1      4.470588            10
#  6: 2010   0.023     -0.5   0.3     15          16        1      4.466667             9
#  7: 2011  -0.003     -0.4  -0.1     23          24        1      5.652174            15
#  8: 2012   0.040     -0.2   0.5     12          13        1      3.750000             7
#  9: 2013   0.046     -0.2   0.8     18          19        1      3.277778             4
# 10: 2014  -0.007     -0.8  -0.2     23          24        1      6.521739            10
# 11: 2015   0.000     -0.9   0.0     26          27        1      5.153846            11
# 12: 2016   0.021     -0.4   0.8     38          39        1      5.263158            14
# 13: 2017   0.027     -0.6   0.6     21          22        1      4.428571            10
# 14: 2018   0.012     -0.3   0.3     23          24        1      4.304348            14
# 15: 2019   0.032     -0.3   1.0     32          33        1      3.906250            17
# 16: 2020  -0.023     -1.4  -0.8     35          36        1      5.542857            17
# 17: 2021   0.059     -0.8   2.1     35          36        1      3.571429            15
# 18: 2022   0.004     -0.6   0.2     45          46        1      4.377778            22
# 19: 2023   0.000     -0.7   0.0     36          37        1      4.138889            14
# 20: 2024   0.024     -0.7   0.8     34          35        1      4.735294            15
# 21: 2025   0.010     -0.4   0.2     25          26        1      4.680000            12

#crypto might be ok
drop_etfs = prices[(volume*close/unadjClose) >1000000 & unadjClose>7 & 
                     (lead1sell_rally/lead1open<2) & category %in% c('equity basket', 'physical commodities') &
                     (((close-low)/(high-low))<.15 ) & (MACD/MACD_slow)< .98  ][
                       order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,symbol,lead1sell_rallydate,
                   hold_max = 1,buy_per_day_max = 1, hold_same_max = F))

# deviant_etfs
#    yr_total_per_held   avg_trade drawdown total_per_drwdn drawdown_days days_traded max_held
# 1:         0.4636364 0.007423581     -1.1        9.272727           783        1396        1
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2004  -0.004     -0.4  -0.2     55          56        1      4.018182            18
#  2: 2005   0.006     -0.3   0.4     66          67        1      3.696970            23
#  3: 2006   0.007     -0.1   0.4     63          64        1      4.206349            27
#  4: 2007   0.008     -0.1   0.5     66          67        1      4.075758            17
#  5: 2008   0.034     -0.5   2.2     64          65        1      4.515625            13
#  6: 2009   0.008     -0.6   0.5     61          62        1      4.557377            15
#  7: 2010   0.013     -0.6   0.8     65          66        1      4.215385            17
#  8: 2011   0.002     -0.7   0.1     57          58        1      4.964912            14
#  9: 2012   0.002     -0.7   0.1     65          66        1      4.015385            15
# 10: 2013   0.012     -0.6   0.8     65          66        1      4.323077            16
# 11: 2014   0.014     -0.3   0.9     63          64        1      4.555556            12
# 12: 2015  -0.012     -1.1  -0.9     71          72        1      4.014085            12
# 13: 2016   0.016     -1.1   0.9     57          58        1      4.701754             9
# 14: 2017   0.012     -0.4   0.8     61          62        1      4.409836            14
# 15: 2018  -0.017     -1.0  -0.9     55          56        1      5.236364            12
# 16: 2019   0.031     -1.0   2.3     73          74        1      3.575342             7
# 17: 2020   0.000     -0.9   0.0     55          56        1      5.163636            14
# 18: 2021   0.002     -0.5   0.1     58          59        1      4.931034            12
# 19: 2022   0.008     -1.1   0.6     70          71        1      3.785714            14
# 20: 2023   0.011     -0.6   0.7     68          69        1      3.911765            14
# 21: 2024  -0.005     -0.8  -0.3     56          57        1      5.125000             8
# 22: 2025   0.007     -0.6   0.4     60          61        1      4.516667            13

deviant = prices[(volume*close/unadjClose) >500000 & unadjClose>7 & lead1sell_rally/lead1open<1.5 & category %in% c('equity basket') &
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
# 1:         0.1759259 0.009958932     -2.8        3.464286           796         994        3
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2005   0.006      0.0   0.0      1           2        1     1.0000000             1
#  2: 2006   0.008      0.0   0.0      5           5        1     0.4000000             1
#  3: 2007   0.035      0.0   0.2      6           7        1     0.3333333             3
#  4: 2008   0.008     -0.8   0.4     50          51        3     3.1000000            19
#  5: 2009   0.028     -0.5   0.6     20          21        3     2.4500000             4
#  6: 2010   0.013      0.0   0.1     11          12        3     2.9090909             5
#  7: 2011  -0.005     -1.1  -0.2     33          34        3     3.5151515            14
#  8: 2012   0.020     -0.5   0.6     30          31        3     3.5333333             4
#  9: 2013   0.017     -0.2   0.4     27          28        3     2.6296296             7
# 10: 2014  -0.001     -1.8  -0.1     55          56        3     4.3818182            16
# 11: 2015   0.006     -1.0   0.4     72          73        3     3.5138889            10
# 12: 2016   0.035     -0.4   1.6     44          45        3     2.7045455             8
# 13: 2017   0.007     -0.3   0.3     48          49        3     3.4375000             8
# 14: 2018  -0.002     -0.7  -0.1     48          49        3     4.2083333            12
# 15: 2019   0.016     -0.7   1.3     77          78        3     2.8441558            16
# 16: 2020  -0.004     -2.8  -0.4     91          92        3     3.2307692            23
# 17: 2021   0.023     -0.8   1.3     57          58        3     3.5087719            13
# 18: 2022  -0.016     -2.1  -1.4     88          89        3     3.4886364            25
# 19: 2023   0.008     -2.0   0.6     75          76        3     3.7333333            12
# 20: 2024   0.030     -1.2   2.7     91          92        3     2.9120879            15
# 21: 2025   0.030     -0.3   1.4     45          46        3     2.3111111            18

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
  merge(deviant, on='date', all=T )%>%
  merge(corr_reverse, on='date', all=T, suffixes = c("deviant",'corr_reverse'))
helds[,sum_held:=rowSums(.SD,na.rm=T),.SDcols=c("n_heldrally","n_heldrevert","n_heldcorr_long","n_helddrop_etfs","n_helddeviant","n_heldcorr_reverse")]
# helds[order(sum_held)]
# cor(helds[,.(n_heldrally,n_heldrevert,n_heldcorr_long,n_helddrop_etfs,n_held)],use = 'pairwise.complete')



outcome_cols <- c("outcomerally", "outcomerevert", "outcomecorr_long", "outcomedrop_etfs", "outcomedeviant","outcomecorr_reverse")
for (j in outcome_cols) {
  set(helds, which(is.na(helds[[j]])), j, 0)
}

setorder(helds, date)

roll_cols <- paste0("roll90_", outcome_cols)
helds[, (roll_cols) := lapply(.SD, function(x) frollsum(x, n = 90, align = "right")), .SDcols = outcome_cols]
cor_matrix <- cor(helds[, ..roll_cols], use = "pairwise.complete.obs")

print(cor_matrix)