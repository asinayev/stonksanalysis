prices=fread("~/datasets/historical_forex.csv")

prices=prices[, .SD[1], by=.(symbol,date)][
     ,.(symbol,date, open, high, low, volume, close, market_cap=0)]


prices[,.N,symbol][,N]%>%hist

prices[symbol %in% prices[,.N,symbol][N>(150), unique(symbol)] & !is.na(lag5close),
          running_var_ratio:=
            frollmean(runVar( close/lag5close, n=25)/(5*runVar(close/lag1close, n=25)), n=100),
          symbol]

prices[,close_today:=ifelse(lead1high>high,high,ifelse(lead1low<low,low,lead1close))]
prices[,trendiness:=frollmean(ifelse(avg_delta>1+sd_from0/5, avg_delta_short-1, ifelse(avg_delta<1-sd_from0/10, 1-avg_delta_short, 0)), n=250), symbol_session]
prices[,lead30close:=shift(close, n = 30, type = "lead"),symbol_session]
prices[,lead1low:=shift(low, n = 1, type = "lead"),symbol_session]

prices[,avg_delta_long:=frollmean(close, n=50, align='right'),symbol_session]
prices[,lag1avg_delta_long:=shift(avg_delta_long, n = 1, type = "lag"),symbol_session]
prices[,no_pos_delta_long:=frollmean(lag1avg_delta_long>1, n=200)==0,symbol_session]

prices[low<lag1close*.98 ][
  order(day_drop_norm, decreasing=F),head(.SD,1),date][lead1low<low]%>%
  with(performance(date,lead1sell_rally/low-1,lead1sell_rallydate-date,symbol,lead1sell_rallydate,hold_less_than=3))



fx_pairs = c('C:AUDUSD',
             'C:AUDZAR',
             'C:AUDCNH',
             'C:AUDSGD',
             'C:AUDNZD',
             'C:AUDCAD',
             'C:AUDJPY',
             'C:AUDHKD',
             'C:AUDCHF',
             'C:CADCHF',
             'C:CADCNH',
             'C:CADJPY',
             'C:CADHKD',
             'C:CHFUSD',
             'C:CHFTRY',
             'C:CHFPLN',
             'C:CHFHUF',
             'C:CHFCZK',
             'C:CHFZAR',
             'C:CHFCNH',
             'C:CHFNOK',
             'C:CHFDKK',
             'C:CHFSEK',
             'C:CHFJPY',
             'C:CNHJPY',
             'C:CNHHKD',
             'C:DKKSEK',
             'C:DKKJPY',
             'C:DKKNOK',
             'C:EURUSD',
             'C:EUROMR',
             'C:EURKWD',
             'C:EURBHD',
             'C:EURQAR',
             'C:EURAED',
             'C:EURSAR',
             'C:EURZAR',
             'C:EURCNH',
             'C:EURRUB',
             'C:EURPLN',
             'C:EURCZK',
             'C:EURHUF',
             'C:EURNZD',
             'C:EURILS',
             'C:EURTRY',
             'C:EURNOK',
             'C:EURDKK',
             'C:EURSGD',
             'C:EURSEK',
             'C:EURMXN',
             'C:EURCAD',
             'C:EURAUD',
             'C:EURJPY',
             'C:EURHKD',
             'C:EURCHF',
             'C:EURGBP',
             'C:GBPUSD',
             'C:GBPSGD',
             'C:GBPCZK',
             'C:GBPHUF',
             'C:GBPPLN',
             'C:GBPZAR',
             'C:GBPCNH',
             'C:GBPDKK',
             'C:GBPNZD',
             'C:GBPTRY',
             'C:GBPNOK',
             'C:GBPMXN',
             'C:GBPSEK',
             'C:GBPCAD',
             'C:GBPAUD',
             'C:GBPJPY',
             'C:GBPHKD',
             'C:GBPCHF',
             'C:HKDJPY',
             'C:KRWUSD',
             'C:KRWJPY',
             'C:KRWHKD',
             'C:KRWGBP',
             'C:KRWEUR',
             'C:KRWCHF',
             'C:KRWCAD',
             'C:KRWAUD',
             'C:MXNJPY',
             'C:NOKSEK',
             'C:NOKJPY',
             'C:NZDUSD',
             'C:NZDCHF',
             'C:NZDCAD',
             'C:NZDJPY',
             'C:SEKJPY',
             'C:SGDJPY',
             'C:SGDHKD',
             'C:SGDCNH',
             'C:USDCHF',
             'C:USDTRY',
             'C:USDBGN',
             'C:USDILS',
             'C:USDZAR',
             'C:USDCNH',
             'C:USDAED',
             'C:USDHKD',
             'C:USDJPY',
             'C:USDCAD',
             'C:USDRUB',
             'C:USDDKK',
             'C:USDPLN',
             'C:USDHUF',
             'C:USDCZK',
             'C:USDMXN',
             'C:USDKRW',
             'C:USDSEK',
             'C:USDSGD',
             'C:USDNOK',
             'C:USDRON',
             'C:USDTRY',
             'C:USDBGN',
             'C:USDILS',
             'C:USDZAR',
             'C:USDCNH',
             'C:ZARJPY')

prices = fx_pairs%>%
  parallel::mclapply(
    stock_history,
    start_date = Sys.Date()-20*365, end_date = Sys.Date()+2, key = POLYKEY, 
    print=F, check_ticker=F,mc.cores = 16)  %>% 
  rbindlist(use.names=TRUE, fill=T)

prices=prices[, .SD[1], by=.(stock, Date)][
  ,.(symbol=stock,date=Date, AdjClose, open, high, low, volume, close=AdjClose, market_cap=0)]
