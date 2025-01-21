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

# prices[,RSI2:=NULL ]
# prices[(symbol %in% prices[!is.na(close-lag1close),.N,symbol][N>5,symbol]) & !is.na(close-lag1close),
#        RSI2:=SMA(pmax(0, close-lag1close) ,n = 2, align='right',fill=NA)/
#          SMA(pmax(0, lag1close-close) ,n = 2, align='right',fill=NA),symbol ]
# 
# prices[,PDM:=NULL ]
# prices[(symbol %in% prices[!is.na(close-lag1close),.N,symbol][N>5,symbol]) & !is.na(close-lag1close),
#        PDM:=SMA(pmax(0, high-lag1high) ,n = 3, align='right',fill=NA),
#        symbol ]
# prices[,NDM:=NULL ]
# prices[(symbol %in% prices[!is.na(close-lag1close),.N,symbol][N>5,symbol]) & !is.na(close-lag1close),
#        NDM:=SMA(pmax(0, lag1low-low  ) ,n = 3, align='right',fill=NA),
#        symbol ]
# prices[,ADX:=NULL ]
# prices[(symbol %in% prices[!is.na((PDM-NDM)/(PDM+NDM)),.N,symbol][N>30,symbol]) & !is.na((PDM-NDM)/(PDM+NDM)),
#        ADX:=EMA((PDM-NDM)/(PDM+NDM) ,n = 30, align='right',fill=NA),
#        symbol ]

lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)
rally_avg(prices,200)
performance_features(prices)
prices=key_etfs(prices,low_corr_thresh=.33)
 
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
# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1:   0.0256 0.02372719     -2.3           519        1056        8
#    year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2008   0.055     -0.2   1.7     31          32        7      4.343750             5
# 2: 2009   0.008     -0.4   0.7     80          81        8      4.387500             8
# 3: 2010   0.013     -0.2   1.0     80          81        7      4.100000             9
# 4: 2011   0.029     -0.6   2.0     67          68        6      4.059701             8
# 5: 2012   0.001     -0.2   0.0     12          13        3      5.416667             4
# 6: 2013   0.016      0.0   0.1      4           5        3      3.250000             2
# 7: 2014   0.053      0.0   0.8     15          16        4      3.866667             4
# 8: 2015   0.026     -0.8   0.9     37          38        7      3.756757             8
# 9: 2016   0.015     -1.6   1.6    110         111        8      5.644068             9
# 10: 2017   0.011     -0.7   0.4     34          35        6      4.058824             4
# 11: 2018   0.050     -0.4   2.7     54          55        7      4.314815             8
# 12: 2019   0.041     -0.4   3.3     82          83        7      5.207317             7
# 13: 2020   0.003     -2.3   0.4    144         145        8      5.468354            20
# 14: 2021   0.028     -0.4   5.0    176         177        8      4.704918            18
# 15: 2022   0.035     -1.4   4.1    115         116        8      4.418803            17


setorder(prices, symbol, date)
rally = prices[volume>1000000 & close>7 & 
         lead1sell_rally/lead1open<2 & 
         close<lag1high & sell_rally_day>10  ][
           order(day_drop_norm, decreasing=F),head(.SD,1),date] %>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol,lead1sell_rallydate,hold_less_than=5))

# revert ETFs
#      avg_year avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.02722222    0.0217     -1.5           250        1018        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
#  1: 2005   0.002      0.0   0.0      2           3        1      6.000000             1
#  2: 2006   0.067      0.0   0.1      1           2        1      3.000000             1
#  3: 2007   0.023      0.0   0.1      3           4        1      1.666667             3
#  4: 2008   0.016     -0.6   0.7     42          43        5      6.333333            21
#  5: 2009   0.033     -0.4   1.1     35          36        5      5.771429            15
#  6: 2010   0.041     -0.2   1.4     35          36        5      4.800000            12
#  7: 2011   0.015     -0.6   0.8     52          53        5      4.519231            18
#  8: 2012   0.030     -0.1   0.5     17          18        3      5.823529             9
#  9: 2013   0.028     -0.1   0.7     25          26        4      5.880000             8
# 10: 2014   0.042     -0.2   1.2     29          30        5      5.758621            10
# 11: 2015   0.028     -0.7   2.0     69          70        5      4.623188            18
# 12: 2016   0.047     -1.0   3.6     77          78        5      6.233766             9
# 13: 2017   0.049     -0.1   1.0     20          21        3      4.750000             6
# 14: 2018   0.007     -1.2   0.5     75          76        5      6.093333            17
# 15: 2019   0.020     -1.3   2.1    105         106        5      6.085714            14
# 16: 2020   0.019     -0.9   3.1    165         166        5      5.993939            27
# 17: 2021   0.012     -1.5   1.5    129         130        5      6.860465            26
# 18: 2022   0.011     -1.1   1.3    119         120        5      6.386555            35


revert = prices[volume>1000000 & close>7 & (lead1sell_rally/lead1open<2)  & 
         (((close-low)/avg_range)<.15 ) & 
           ((high/close) > avg_range*2 | avg_delta<(.99-(.01*lever))
         )
    ][order( day_drop_norm, decreasing=F),head(.SD,1),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol,lead1sell_rallydate,hold_less_than=5))

# Corr long etfs

# avg_year avg_trade drawdown drawdown_days days_traded max_held
# 1:  0.02325  0.021662     -1.9           308        1087        8
#    year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2007   0.029      0.0   0.1      5           6        1      2.833333             3
# 2: 2008   0.044     -0.2   1.6     35          36        7      4.628571            20
# 3: 2009   0.019     -0.1   0.2     12          13        2      4.000000             7
# 4: 2010   0.036     -0.2   1.7     48          49        7      4.229167            14
# 5: 2011   0.027     -0.2   2.0     74          75        6      4.094595            30
# 6: 2012  -0.009     -0.3  -0.2     17          18        6      4.823529             9
# 7: 2013   0.021     -0.2   0.6     29          30        5      3.586207            11
# 8: 2014   0.058     -0.2   1.7     29          30        8      5.600000            20
# 9: 2015   0.014     -0.8   0.9     66          67        8      4.926471            25
# 10: 2016   0.032     -1.9   3.3    103         104        8      5.728972            24
# 11: 2017   0.011     -0.6   0.5     41          42        8      5.930233            12
# 12: 2018  -0.002     -1.1  -0.3    110         111        8      6.238938            28
# 13: 2019   0.029     -0.8   3.0    105         106        8      5.537736            21
# 14: 2020   0.030     -1.4   3.4    115         116        8      5.232759            46
# 15: 2021   0.017     -0.9   2.5    148         149        8      5.855263            34
# 16: 2022   0.016     -1.3   2.2    134         135        8      5.074074            46

corr_long = prices[volume>1000000 & close>7 & 
         (avg_delta_short<.975) & lagging_corr_long> .35][
           order(day_drop_norm, decreasing=F),head(.SD,1),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol, lead1sell_rallydate, hold_less_than = 5))

# Drop ETFs

# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.01255556 0.01637348     -1.4           935         757        5
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2005  -0.018     -0.1  -0.1      3           4        2     10.000000             2
# 2: 2006  -0.028     -0.2  -0.1      4           5        2      5.500000             4
# 3: 2007   0.019     -0.2   0.2     13          14        3      3.000000             7
# 4: 2008   0.017     -0.5   0.5     29          30        5      5.103448            14
# 5: 2009   0.010     -0.3   0.2     19          20        3      3.736842            14
# 6: 2010   0.023     -0.1   0.5     22          23        4      3.954545            15
# 7: 2011   0.017     -0.2   0.9     54          55        5      4.537037            30
# 8: 2012   0.023     -0.1   0.7     28          29        5      4.500000            13
# 9: 2013   0.013     -0.2   0.5     38          39        5      3.605263            22
# 10: 2014   0.024     -0.2   0.7     31          32        5      4.806452            20
# 11: 2015   0.025     -0.1   1.6     66          67        5      4.575758            30
# 12: 2016  -0.002     -1.1  -0.1     62          63        5      4.387097            37
# 13: 2017   0.018     -0.2   0.4     24          25        3      3.666667            11
# 14: 2018   0.016     -0.1   0.8     50          51        5      3.680000            29
# 15: 2019   0.019     -0.2   1.1     55          56        3      4.327273            27
# 16: 2020   0.025     -0.9   2.0     80          81        5      4.912500            46
# 17: 2021   0.019     -0.2   1.9     98          99        5      4.010204            47
# 18: 2022   0.006     -1.4   0.4     63          64        5      4.190476            38

drop_etfs = prices[volume>1000000 & close>7 & !short &
                     (avg_delta_short < .99-avg_range/close/2 ) ][
           order(day_drop_norm, decreasing=F),head(.SD,1),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol, 
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