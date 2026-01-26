require(tidyquant)
require(data.table)
require(rpart)
require(ggplot2)

setwd('~/stonksanalysis')
source("polygon.R", local=T)
source("implement/features.R", local=T)
source("research/performance.R", local=T)
POLYKEY = Sys.getenv('POLYGONKEY')

# Get data from polygon instead
prices=lapply(2022:2011, #16:22
              function(yr){
                x=fread(paste0("/home/rstudio/datasets/stocks_by_yr/",yr,".csv.gz"), colClasses = c(cik = "character"))
                if(nrow(x)<1000){
                  x=data.table(read.csv(paste0("/home/rstudio/datasets/stocks_by_yr/",yr,".csv.gz"), colClasses = c(cik = "character")))
                  x[,date := as.IDate(date)]
                  x[,list_date := as.IDate(list_date)]
                }
                subset(x, type %in% c('CS','PF',''))
              })
prices = rbindlist(prices, use.names=T, fill=T)
setnames(prices, 'stock', 'symbol')

prices = prices[ volume*open*close > 0]
setorder(prices, symbol, date)
lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)
performance_features(prices)

prices[order(ifelse(is.na(market_cap),ifelse(is.na(weighted_shares_outstanding),share_class_shares_outstanding*unadjClose,weighted_shares_outstanding*close),market_cap),decreasing=T),
       cap_order:=seq_len(.N),date]

prices[order(market_cap,decreasing=T),
       cap_order0:=seq_len(.N),date]

#prices=prices[close*volume>500000]
#gc()
#x=get_financials(prices[symbol<'g'],id_type='cik')
#y=get_financials(prices[symbol>'g' & symbol<'o'],id_type='cik')
#z=get_financials(prices[symbol>'o'],id_type='cik')
#prices=rbind(x,y,z)

prices[(symbol_session %in% prices[!is.na(close),.N,symbol_session][N>50,symbol_session]) & !is.na(close),
       dema_delta:=DEMA(close/lag1close,   n = 25),symbol_session ]

#####bandlong
# prices[!is.na(lag1close),
#        c('lower','avg','upper','pctB'):= data.frame(BBands(lag1close, n = 30, EMA, sd=2.5)),
#        symbol ]
# prices[(close<low*1.01 | close<low_running) & day_delta<.8 & close>7
#        & lag1volume>75000, 
#         .(mean(lead1open/close, na.rm=T), median(lead1open/close,na.rm=T),.N),
#        year(date)][order(year)]

#At close, buy stocks that fell 15% and closed at the day's low or 50D low of close prices
#####

#####updown
# prices[
#   open/lag1close> 1.05 & close/open<.9  & close>7 & 
#     volume>75000  & volume*lag1close<1000000
#   ,.(mean(lead1open/close, na.rm=T), .N), 
#   .(year(date))][order(year)] 

#At close, buy stocks that climbed last night and fell today, setting limit to 95% of the open
#####

#####updownmorn
# prices[
#   close/open>1.025 & spy_future_night_delta>.99 & 
#     volume%between%c(10000,20000) & close>5 & lead1open/close<.975]%>%
#   with(performance(date,lead1close/lead1open-1,1,symbol))

# prices[
#   volume/avg_volume <.75 & spy_future_night_delta>.99 & 
#     volume%between%c(10000,20000) & close>5 & lead1open/close<.975]%>%
#   with(performance(date,lead1close/lead1open-1,1,symbol))

# prices[
#   (volume/avg_volume <.75 | close/open>1.025) & spy_future_night_delta>.99 & 
#     volume%between%c(10000,20000) & close>5 & (lead1open/close)<.975]%>%
#   with(performance(date,(lead1close/lead1open)-1,1,symbol))

#At open, buy stocks that climbed yesterday but fell overnight today unless the index fell overnight
#####

#####overbought
# prices[
#   (close/open)>1.2 & close>7 & (open/lag1close)>1  &
#     vp_order<3000][order(day_rise_norm,decreasing=T),.SD[1],date]%>% 
#   with(performance(date,1-lead1sell_lowclose/lead1open,1,symbol,lead1sell_lowclosedate,hold_less_than=5))


# At open, sell stocks that climbed yesterday too much
#####


###### volumeshort
# prices[lag1volume/volume_avg>7.5 & lag1_day_delta>.975 & night_delta>1.01  & open>7.5 &
#          lag1volume%between%c(10000,100000),
#        .(mean(day_delta,na.rm=T),.N), year(date)][order(year)]
#
######


#####volumelong
#      avg_year   avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.01088889 0.009758657     -1.2           804         971        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2004   0.018      0.0   0.4     19          20        2      4.894737            19
# 2: 2005   0.013     -0.1   0.4     30          31        4      5.133333            29
# 3: 2006   0.011     -0.1   0.3     30          31        3      6.133333            29
# 4: 2007  -0.010     -0.5  -0.3     36          37        5      8.305556            35
# 5: 2008   0.019     -0.4   1.1     56          57        4      5.357143            53
# 6: 2009  -0.010     -0.7  -0.2     24          25        5      7.250000            22
# 7: 2010   0.011     -0.2   0.3     27          28        3      5.185185            26
# 8: 2011   0.005     -0.3   0.2     34          35        3      4.411765            32
# 2: 2012   0.004     -0.3   0.1     27          28        3      5.703704            26
# 3: 2013   0.008     -0.1   0.2     26          27        2      6.461538            26
# 4: 2014   0.006     -0.2   0.4     60          61        5      5.416667            58
# 5: 2015   0.007     -0.3   0.3     49          50        5      6.428571            46
# 6: 2016   0.008     -0.3   0.2     30          31        3      5.200000            29
# 7: 2017   0.003     -0.2   0.1     36          37        4      6.472222            34
# 8: 2018   0.005     -0.4   0.3     62          63        5      5.854839            58
# 2: 2019   0.001     -0.3   0.0     20          21        2      3.850000            20
# 3: 2020  -0.009     -1.4  -0.4     45          46        5      5.311111            43
# 4: 2021   0.005     -0.9   0.3     54          55        3      4.777778            53
# 5: 2022   0.016     -0.5   0.8     47          48        4      4.468085            46
# 6: 2023   0.006     -0.2   0.2     36          37        4      5.277778            34
# 7: 2024  -0.001     -0.2   0.0     21          22        2      7.333333            21
# 8: 2025   0.006     -0.3   0.3     45          46        5      6.822222            43

# works well until 2023, but not after
prices[lead1sell_rally/lead1open<1.5 & 
         (volume*close/unadjClose) >100000 & unadjClose>25 & 
         volume>=max_volume & 
         avg_delta_short<.99 & 
         vp_order>cap_order0 &
         (close-low)/avg_range<.1 ][
           order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,
                   symbol,lead1sell_rallydate,hold_max = 5,buy_per_day_max = 1, hold_same_max = F))


# 1: 2004   0.004     -0.2   0.1     20          21        3      5.400000            20
# 2: 2005   0.015     -0.4   0.6     40          41        3      4.175000            35
# 3: 2006   0.001     -0.5   0.0     52          53        4      5.634615            47
# 4: 2007   0.019     -0.2   1.0     51          52        5      5.882353            47
# 5: 2008   0.005     -1.2   0.3     63          64        5      5.714286            52
# 6: 2009  -0.009     -0.8  -0.2     17          18        4      5.764706            17
# 7: 2010   0.000     -0.9   0.0     30          31        4      6.400000            28
# 8: 2011   0.003     -0.8   0.1     26          27        4      4.153846            26
# 2: 2012   0.004     -0.1   0.1     15          16        2      5.533333            15
# 3: 2013   0.017     -0.1   0.4     24          25        2      4.958333            22
# 4: 2014   0.009     -0.3   0.3     31          32        4      4.419355            26
# 5: 2015   0.008     -0.3   0.3     38          39        4      4.236842            32
# 6: 2016   0.007     -0.2   0.2     32          33        4      4.343750            30
# 7: 2017   0.010     -0.1   0.2     23          24        2      5.173913            23
# 8: 2018   0.007     -0.3   0.2     36          37        4      6.194444            31
# 2: 2019  -0.006     -0.4  -0.1     17          18        2      5.647059            16
# 3: 2020   0.015     -0.9   0.7     49          50        4      4.040816            44
# 4: 2021   0.020     -0.3   0.8     39          40        5      5.153846            33
# 5: 2022   0.017     -0.1   0.8     46          47        3      3.956522            44
# 6: 2023  -0.013     -0.7  -0.4     29          30        4      5.241379            28
# 7: 2024   0.024     -0.6   0.7     28          29        2      4.607143            25
# 8: 2025   0.011     -0.5   0.4     36          37        4      4.694444            32
# new version works without market cap 
prices[lead1sell_rally/lead1open<1.5 & (volume*close/unadjClose) >100000 & unadjClose>25 & 
         volume>=max_volume & volume<lag1volume*1.5 &
         close<lag1close*.975 &
         vp_order<500 ][
           order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,
                   symbol,lead1sell_rallydate,hold_max = 5,buy_per_day_max = 1, hold_same_max = F))




#####
# Regression strategy
# sq=function(x)x^2
# prices[,reg_predict := as.numeric(32)]
# prices[,reg_predict := NA]
# regression_features(prices)
# for (yr in 2008:2021 ){
#   IS = prices[year(date) %between% c(yr-3, yr-1) & volume>75000 & close>7 ]
#   lm1 = lm(future_day_delta ~
#              day_delta + night_delta + day_fall + day_rise 
#            ,IS, weights = (IS$date-min(IS$date))/as.integer(max(IS$date-min(IS$date)))
#   )
#   print(yr)
#   print(round(lm1$coefficients,3))
#   prices[year(date)==yr, reg_predict:=predict(lm1,data.frame(.SD))  ]
#   gc()
# }
# 
# 
# IS = prices[date>Sys.Date()-3*365 & date<Sys.Date()-30 & volume>75000 & close>7]
# lm1 = lm(future_day_delta ~
#            day_delta + night_delta + day_fall + day_rise
#          ,IS, weights = (IS$date-min(IS$date))/as.integer(max(IS$date-min(IS$date)))
# )
# prices[year(date)==2022,
#        reg_predict:=predict(lm1,data.frame(.SD))  ]
# 
# prices[,reg_predict:=ifelse(is.na(reg_predict),1,reg_predict)]
# prices[volume>75000 & close>7,threshold:=pmin(quantile(reg_predict,.001,type=1),.995), date]

# year        V1   N
# 1: 2015 1.0015948 320
# 2: 2016 1.0033732 153
# 3: 2017 0.9783223  34
# 4: 2018 0.9636028 101
# 5: 2019 0.9748256 652
# 6: 2020 0.9776544 733
# 7: 2021 0.9858612 677
# 8: 2022 0.9796050  85
# prices[reg_predict<threshold  & #!day_delta>1.2 &
#          volume>75000 & close>7]%>% 
#   with(performance(date,1-lead1close/lead1open,1,symbol))
# 
# prices[!is.na(future_day_delta) & reg_predict<threshold  & volume>75000 & close>7][order(date, symbol)][
#   ,.(date, MA = EMA(future_day_delta,na.rm=T,50))] %>% with(plot(date, MA, type='l', ylim=c(.8,1.2)))
# x_ <- c(1, .99, 1.01,.95,1.05) %>% lapply( function(x)abline(h=x))




###### Correlated long and short
rally_avg(prices,100)

#    avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1:    0.034 0.02469799     -2.9           886         762        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2004   0.015     -0.6   0.7     46          47        5      5.630435            13
# 2: 2005  -0.023     -0.9  -0.7     30          31        5      4.100000             7
# 3: 2006   0.019     -0.8   0.8     40          41        5      3.850000            11
# 4: 2007   0.043     -0.4   2.1     48          49        5      4.895833            12
# 5: 2008  -0.004     -3.0  -0.5    143         144        5      4.538462            61
# 6: 2009   0.026     -3.1   1.4     54          55        5      4.388889            25
# 7: 2010   0.021     -1.1   0.6     29          30        5      2.689655             8
# 8: 2011   0.013     -0.5   0.5     38          39        5      4.184211            16
# 2: 2012   0.020     -0.2   0.7     34          35        5      2.558824            11
# 3: 2013   0.022     -0.5   0.7     32          33        5      5.406250             9
# 4: 2014   0.028     -0.1   1.7     61          62        5      3.442623            19
# 5: 2015   0.029     -0.4   2.0     70          71        5      3.500000            25
# 6: 2016   0.016     -0.8   0.9     57          58        5      3.543860            22
# 7: 2017   0.068      0.0   0.9     13          14        3      1.538462             5
# 8: 2018   0.021     -0.3   0.9     42          43        5      3.952381            18
# 2: 2019   0.011     -0.4   0.6     52          53        5      2.923077            19
# 3: 2020   0.008     -1.7   0.4     48          49        5      2.520833            30
# 4: 2021  -0.002     -0.9  -0.2     94          95        5      5.617021            30
# 5: 2022   0.010     -1.8   1.0    101         102        5      4.673267            44
# 6: 2023  -0.013     -2.8  -1.1     85          86        5      4.905882            31
# 7: 2024   0.012     -3.0   0.9     76          77        5      4.250000            23
# 8: 2025   0.011     -1.7   0.8     69          70        5      4.405797            22

prices[(volume*close/unadjClose) >100000 & unadjClose>7 & cap_order<1500 &
         avg_volume>1000000 &
         close<lag1high & sell_rally_day>4 & 
         avg_delta<.98][
           order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,
                   symbol,lead1sell_rallydate,hold_max = 5,buy_per_day_max = 1, hold_same_max = F))

# prices[close>7 & avg_volume>500000 & 
#          close>lag1high & sell_rally_day<2 & 
#          avg_delta_short>1.1][
#          ][order(day_rise_norm, decreasing=T),head(.SD,1),date]%>%
#   with(performance(lead1date,
#                    1-lead1sell_lowclose/lead1open,
#                    lead1sell_lowclosedate-lead1date,symbol,
#                    lead1sell_lowclosedate,
#                    hold_less_than = 5))


############
# nightbot
# 
# prices[close>5 & volume>100000 & lead1open/close>1.15 & spy_future_night_delta<1.005 ]%>%
#   with(performance(date,1-lead1sell_lowclose/lead1open,
#                    1,symbol))

##############
# megacap
#     avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.01305263 0.01387347     -1.1           522         919        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2004   0.024      0.0   0.9     37          38        5      2.945946            10
# 2: 2005   0.008     -0.1   0.4     52          53        5      4.000000            10
# 3: 2006   0.016     -0.1   0.9     55          56        5      3.290909            10
# 4: 2007   0.003     -0.3   0.2     68          69        5      4.500000            15
# 5: 2008   0.001     -1.8   0.2    137         138        5      4.138686            28
# 6: 2009   0.026     -2.1   2.9    112         113        5      3.607143            26
# 7: 2010   0.015     -0.3   1.2     81          82        5      3.580247            20
# 8: 2011   0.008     -0.5   0.6     73          74        5      4.054795            19
# 2: 2012   0.017     -0.5   0.7     44          45        4      3.159091            13
# 3: 2013   0.008     -0.3   0.5     69          70        5      2.753623            13
# 4: 2014   0.004     -0.2   0.2     43          44        5      4.813953             8
# 5: 2015   0.018     -0.2   0.7     39          40        5      4.128205            11
# 6: 2016   0.014     -0.3   0.5     39          40        5      4.743590            11
# 7: 2017   0.014     -0.1   0.8     55          56        5      3.854545            11
# 8: 2018   0.009     -0.2   0.7     79          80        5      4.632911            19
# 2: 2019   0.008     -0.3   0.4     58          59        5      3.482759            19
# 3: 2020   0.016     -0.6   1.8    112         113        5      4.767857            29
# 4: 2021   0.012     -0.3   1.2    102         103        5      3.519608            25
# 5: 2022   0.005     -0.7   0.7    125         126        5      4.104000            29
# 6: 2023   0.004     -0.8   0.4     83          84        5      3.698795            20
# 7: 2024   0.003     -1.1   0.2     97          98        5      6.113402            14
# 8: 2025   0.015     -0.4   1.6    109         110        5      3.669725            27

#works until 2025 with a dip in the end of 2008
prices[avg_delta_short<avg_delta*.985 &  
         cap_order<50 & lagging_corr_long>.7 & 
         lead1sell_rally/lead1open<1.5][
           order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,
                   symbol,lead1sell_rallydate,hold_max = 5,buy_per_day_max = 1, hold_same_max = F))

# New version works without market cap
# 1: 2004  -0.035     -2.8  -2.4     68          69        5      5.029412            18
# 2: 2005   0.002     -2.8   0.1     38          39        4      3.789474            12
# 3: 2006   0.013     -2.7   0.4     31          32        3      4.290323            10
# 4: 2007   0.011     -2.4   0.4     37          38        5      5.432432            14
# 5: 2008   0.030     -1.8   3.5    117         118        5      4.196581            23
# 6: 2009   0.026     -1.2   1.5     57          58        5      4.245614            15
# 7: 2010  -0.016     -0.8  -0.6     39          40        5      6.256410            12
# 8: 2011   0.000     -1.2   0.0     55          56        5      4.527273            12
# 1: 2011   0.005     -1.2   0.3     59          60        5      4.254237            14
# 2: 2012   0.018     -0.8   0.4     20          21        5      2.200000             6
# 3: 2013  -0.005     -0.6  -0.1     15          16        4      4.400000             5
# 4: 2014   0.017     -0.4   0.7     40          41        5      6.475000            11
# 5: 2015   0.008     -0.4   0.3     39          40        5      7.589744            12
# 6: 2016   0.018     -0.4   0.8     44          45        5      8.636364            11
# 7: 2017   0.013     -0.2   0.4     31          32        5      5.096774             8
# 8: 2018   0.019     -0.3   1.2     60          61        5      3.966667            14
# 2: 2019   0.015     -0.3   0.6     37          38        5      3.000000             7
# 3: 2020   0.031     -0.8   2.4     78          79        5      3.666667            25
# 4: 2021  -0.007     -1.6  -0.6     83          84        5      7.469880            23
# 5: 2022   0.004     -0.8   0.4     95          96        5      5.694737            22
# 6: 2023   0.010     -0.6   0.4     35          36        5      4.742857            14
# 7: 2024   0.018     -0.7   1.9    105         106        5      4.104762            20
# 8: 2025   0.030     -0.8   2.9     99         100        5      4.080808            18
prices[avg_delta_short<.975 &  
         vp_order<25 & 
         lead1sell_rally/lead1open<1.5][
           order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,
                   symbol,lead1sell_rallydate,hold_max = 5,buy_per_day_max = 1, hold_same_max = F))

#############
# earners
#############
# avg_year  avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.02171429 0.01861472     -0.8           411         707        5
# year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2009   0.023      0.0   0.1      5           6        2             1             1
# 2: 2010   0.048     -0.1   0.6     13          14        5             1             1
# 3: 2011   0.032     -0.5   1.3     40          41        5             1             5
# 4: 2012   0.026      0.0   0.8     30          31        5             1             3
# 5: 2013   0.021      0.0   0.6     27          28        5             1             2
# 6: 2014   0.005     -0.2   0.4     72          73        5             1             6
# 7: 2015  -0.001     -0.5  -0.1     49          50        5             1             4
# 8: 2016   0.011     -0.5   0.6     50          51        5             1             5
# 9: 2017   0.039     -0.1   1.6     41          42        5             1             2
# 10: 2018   0.029     -0.3   0.9     31          32        4             1             3
# 11: 2019   0.012     -0.1   0.6     48          49        5             1             6
# 12: 2020   0.019     -0.8   2.4    122         123        5             1            12
# 13: 2021   0.014     -0.7   1.3     93          94        5             1             9
# 14: 2022   0.026     -0.3   1.8     72          73        5             1            10

prices[lead1sell_rally/lead1open<1.5 & vp_order<1000 & (market_cap>10^9 | is.na(market_cap)) &
         (  (low<running_low*1.001) | 
              (avg_delta_short<avg_delta*.98)) & 
         (mid_eps/close) >.15 &  eps_unit=="USD / shares"  ][
           order(date,day_drop_norm/sd_from0, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,
                   symbol,lead1sell_rallydate,hold_max = 3,buy_per_day_max = 1, hold_same_max = F))


prices[close>7 & avg_volume>250000 & #is.na(in_split_range) &
         avg_delta<1 & avg_delta_short<.995 &
         (mean_eps/close) >.05 &  eps_unit=="USD / shares"  ][
           order(avg_delta, decreasing=F),head(.SD,1),date][,.(lead1open[1],lead300close[1]/lead1open[1],date[1]),.(year(date),symbol)][order(year)][,.(mean(V2,na.rm=T),.N)]

bigcaps = prices[(volume*close/unadjClose) >500000 & unadjClose>7 & cap_order<250]
bigcaps[,bigcap_avg_delta:=mean(avg_delta,na.rm=T),date]
bigcaps[,bigcap_avg_delta_short:=mean(avg_delta_short,na.rm=T),date]

#############
# bigcap_short

# bigcaps[(avg_delta>1.0075 | avg_delta>bigcap_avg_delta*1.0075) & 
#           (avg_delta_short>1.015 | avg_delta_short>bigcap_avg_delta_short*1.015) &
#           lead1sell_lowclose/lead1open>.5][
#             order(day_rise_norm,decreasing=T),head(.SD,1),date] %>%
#   with(performance(date,1-lead1sell_lowclose/lead1open,lead1sell_lowclosedate-date,symbol,lead1sell_lowclosedate,hold_less_than = 5))


#############
# bigcap_long
#       avg_year   avg_trade drawdown drawdown_days days_traded max_held
# 1: 0.009526316 0.009442446     -1.3           607        1131        5
#     year average drawdown total trades days_traded max_held avg_days_held stocks_traded
# 1: 2004   0.005     -0.2   0.3     57          58        4      3.070175            34
# 2: 2005   0.016     -0.1   0.8     54          55        5      3.870370            29
# 3: 2006  -0.012     -0.8  -0.6     52          53        5      5.288462            35
# 4: 2007   0.014     -0.8   0.9     63          64        5      2.984127            33
# 5: 2008   0.005     -1.5   0.9    162         163        5      4.055556            68
# 6: 2009   0.021     -0.7   2.6    120         121        5      3.966667            59
# 7: 2010   0.007     -0.3   0.4     50          51        4      3.120000            24
# 8: 2011   0.000     -0.8   0.0     60          61        4      3.283333            32
# 1: 2011  -0.006     -0.8  -0.3     54          55        4      3.314815            31
# 2: 2012   0.019     -0.6   0.7     39          40        4      3.205128            21
# 3: 2013   0.021     -0.1   1.3     62          63        3      2.919355            25
# 4: 2014   0.005     -0.3   0.3     67          68        5      4.373134            24
# 5: 2015   0.009     -0.2   0.5     50          51        4      3.780000            31
# 6: 2016   0.012     -0.3   0.6     47          48        5      4.063830            25
# 7: 2017   0.015      0.0   0.2     15          16        2      2.533333             9
# 8: 2018   0.000     -0.3   0.0     43          44        5      4.860465            20
# 2: 2019   0.011     -0.6   0.7     62          63        5      4.403226            26
# 3: 2020   0.023     -0.4   2.8    121         122        5      2.702479            45
# 4: 2021   0.001     -1.1   0.1    108         109        5      4.342593            43
# 5: 2022   0.016     -1.1   2.0    126         127        5      3.992063            46
# 6: 2023   0.010     -0.4   0.6     58          59        4      3.448276            30
# 7: 2024   0.005     -0.5   0.4     74          75        5      4.608108            32
# 8: 2025   0.011     -0.3   1.1    103         104        5      4.339806            37

bigcaps[ lead1sell_rally/lead1open<1.5 &
           ((avg_delta>1.0025 & avg_delta_short<.985) | (close>open*1.05 & avg_delta_short<1) )][
             order(date,-volume*close/unadjClose, decreasing=F)] %>%
  with(performance(lead1date,lead1sell_rally/lead1open-1,lead1sell_rallydate-lead1date,
                   symbol,lead1sell_rallydate,hold_max = 5,buy_per_day_max = 1, hold_same_max = F))


################
# night_rebound
################
# executed directly, not via R

prices[volume>500000 & close>7 & close/open<.825 & close<lag5close & 
         lead1sell_rally/lead1open<1.5][
           order(date,close/open, decreasing=F)] %>%
  with(performance(lead1date,lead1open/close-1,0,
                   symbol,lead1date,hold_max = 1,buy_per_day_max = F, hold_same_max = F))


###########
# No working strategies here yet


# stock splits
all_splits= prices$symbol %>% unique %>%
  parallel::mclapply(
    stock_splits,
    key=POLYKEY,
    mc.cores = 16) %>% 
  rbindlist(use.names=TRUE, fill=T)

merge(prices,
      all_splits[,.(date=as.Date(execution_date),split_from,split_to,symbol=ticker)])


