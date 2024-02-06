require(tidyquant, quietly = T)
require(data.table, quietly = T)

setwd('~/stonksanalysis')
source("implement/imports.R", local=T)
source("research/performance.R", local=T)
source("implement/get_data.R", local=T)

days_to_look_at = as.Date(as.Date("2021-04-20"):Sys.Date(),origin='1970-01-01')

# just_news = as.Date(as.Date("2022-04-20"):as.Date("2022-04-29"),origin='1970-01-01') %>%
#   lapply(get_newsday, key=POLYKEY) %>%
#   rbindlist(use.names=TRUE, fill=T)

just_news = parallel::mclapply(
  days_to_look_at,
  get_prev_day_news, key=POLYKEY,mc.cores = 16,
  full_prevday=F
) %>%
  rbindlist(use.names=TRUE, fill=T)

setorder(prices, symbol, date)
prices = prices[!is.na(volume) & !is.na(close) & !is.na(open)]

lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)
prices[,lead1sell_rally:= shift(sell_rally,1,type='lead'),symbol]
prices[,lead1sell_rallydate:= shift(sell_rally_date,1,type='lead'),symbol]

news_moves = just_news %>%
  clean_news %>%
  merge(prices, by=c('date','symbol'), all.x=T)

# x=sapply(unique(floor_date(news_moves$date,'month')),function(yrmth) {
#   top_authors = news_moves[order(close/lag1close)][close>6 & date<as.Date(yrmth),head(.SD,5),.(date,author)][, 
#                            .(median(lead5close/lead1open),.N),author][
#                              V1>1.01 & N>50, author]
#   news_moves[close>6 & floor_date(date,'month')==as.Date(yrmth) & author %in% top_authors,
#                    head(.SD,5),date][,.(as.Date(yrmth),mean(lead1sell_rally/lead1close,na.rm=T),.N)]
# })
# x = data.table(t(x))
# x[,.(sum(as.numeric(V2)*unlist(N),na.rm=T)/sum(unlist(N)),sum(unlist(N)))]

byword = news_moves[!sapply(keywords, is.null),
                    .(keywords=unlist(keywords) ),
                    .(id, delta=lead1sell_rally/lead1open, overnight_delta=lead1open/close,
                      prev_delta=close/open, prev_dol_vol = close*volume, open,
                      symbol, single_ticker, market_cap, date, publisher.name, title)]

byword[log(market_cap)<21 & keywords %in%c('Health', 'Partnerships', 'Press releases') & publisher.name=='GlobeNewswire Inc.'  & overnight_delta>1.01,
       .(mean(delta,na.rm=T),
         median(delta,na.rm=T),
         length(unique(paste(date,symbol)))),.(year(date),month(date) )][order(year,month,decreasing = T)]
# short penny stocks with GlobeNewswire's "Health" keywords
pennyshort_trades = byword[log(market_cap)<21 & keywords %in%c('Health', 'Partnerships', 'Press releases') & publisher.name=='GlobeNewswire Inc.'  & overnight_delta>1.01,
                           .(date,ticker,open,delta)]
pennyshort_hours = get_hours_for_stocks(pennyshort_trades$ticker,
                                        start_date=min(pennyshort_trades$date),
                                        end_date=Sys.Date(),
                                        key=POLYKEY)
merge(pennyshort_trades, pennyshort_hours,
      by.x=c('ticker','date'),by.y=c('stock','bar_date'))[
        ,.(mean(`9`/open,na.rm=T),
           mean(`10`/open,na.rm=T),
           mean(`11`/open,na.rm=T),
           mean(`14`/open,na.rm=T),
           mean(`15`/open,na.rm=T),
           mean(delta,na.rm=T))]

byword[log(market_cap)<21 & publisher.name=='PennyStocks' & overnight_delta>1.01,
       .(mean(delta,na.rm=T),
         median(delta,na.rm=T),
         length(unique(paste(date,ticker)))),.(year(date), month(date) )][order(month,decreasing = T)]
# short PennyStocks penny stocks with upward movement >1%

pennyshort_trades = byword[log(market_cap)<21 & publisher.name=='PennyStocks' & overnight_delta>1.01,
                           .(date,ticker,open,delta)]
pennyshort_hours = get_hours_for_stocks(pennyshort_trades$ticker,
                                        start_date=min(pennyshort_trades$date),
                                        end_date=Sys.Date(),
                                        key=POLYKEY)
merge(pennyshort_trades, pennyshort_hours,
      by.x=c('ticker','date'),by.y=c('stock','bar_date'))[ !is.na(AdjClose_9)
                                                           ,.(mean(open/Open_9,na.rm=T),
                                                              mean(AdjClose_9/open,na.rm=T),
                                                              mean(AdjClose_10/open,na.rm=T),
                                                              mean(AdjClose_11/open,na.rm=T),
                                                              mean(AdjClose_12/open,na.rm=T),
                                                              mean(AdjClose_13/open,na.rm=T),
                                                              mean(AdjClose_14/open,na.rm=T),
                                                              mean(AdjClose_15/AdjClose_8,na.rm=T),
                                                              mean(Open_16/open,na.rm=T),
                                                              mean(delta,na.rm=T),.N)]


byword[log(market_cap)<21 & publisher.name=='Benzinga' &
         keywords%in%c('Small Cap','Penny Stocks') &
         # overnight_delta>1.01,
       .(mean(delta,na.rm=T),
         median(delta,na.rm=T),
         length(unique(paste(date,ticker)))),.(year(date),month(date) )][order(year,month,decreasing = T)]
# short Benzinga's penny stocks with Penny Stocks and Small Cap keywords and upward movement >1%


byword[
  ,.(mean(abs(delta-1),na.rm=T),
    mean(delta,na.rm=T),
    median(delta,na.rm=T),
    length(unique(paste(date,symbol)))),.(publisher.name, keywords)][order(V1,decreasing = T)][V4>1000]

unnest_tokens(news_moves[!is.na(single_ticker),.(date,ticker,title, delta=c/o,publisher.name)], bigram, title)[
  bigram=='daily j']

##
## WINNING
##
news_moves[grepl('earning', title, ignore.case = T) &
           grepl('revenue', title, ignore.case = T) &
           publisher.name=='Zacks Investment Research' &
           avg_volume>50000 & volume>50000 & close>5 & 
           (open-close > avg_range/3 )
           #& !is.na(single_ticker)
           ,
           .(delta=max(close/open), avg_delta=max(avg_delta)),
           .(date,symbol,lead1sell_rally,lead1open)][order(-avg_delta, decreasing=T),head(.SD,5),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,
                   1,symbol))


news_moves[avg_volume>75000 & market_cap<10000000000 &
             !is.na(single_ticker) &
           grepl('(new|announce|declare|authori).*(repurchase|buyback)', title, ignore.case = T),
           max(date),
           .(date,symbol,lead1open,lead1sell_rally,market_cap)][, .(mean(lead1sell_rally/lead1open),.N), .(year(date),month(date))][order(year,month)]
