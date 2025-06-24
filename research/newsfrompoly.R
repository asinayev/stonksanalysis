require(tidyquant, quietly = T)
require(data.table, quietly = T)
require(ggplot2)

setwd('~/stonksanalysis')
source("implement/imports.R", local=T)
source("research/performance.R", local=T)
source("implement/get_data.R", local=T)

days_to_look_at = as.Date(as.Date("2022-04-20"):Sys.Date(),origin='1970-01-01')

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

setorder(prices, symbol, date)
lag_lead_roll(prices, corr_window=100, roll_window=25, short_roll_window=5)
rally(prices)
performance_features(prices)

just_news[,.N,.(date=paste(year(date),stringr::str_pad(month(date ),2, pad="0") ),publisher.name)]%>%
  ggplot( aes(x = date, y = N, color = publisher.name, group = publisher.name)) +
  geom_line() +
  labs(title = "News Items per Day by Publisher",
       x = "Date",
       y = "Number of News Items") +
  theme_bw()  # Adjust theme as desired

news_moves = just_news %>%
  clean_news %>%
  merge(prices, by=c('date','symbol'), all.x=T)

news_amount = just_news %>% tidyr::unnest(cols=c(tickers))
news_amount = data.table(news_amount)[publisher.name%in% c('GlobeNewswire Inc.','Benzinga', 'The Motley Fool', 'Seeking Alpha', 'MarketWatch'),
                                      .(news_items=.N),.(date,symbol=tickers)]
news_amount = merge(prices, news_amount, by=c('date','symbol'), all.x=T)
setorder(news_amount, symbol, date)
news_amount[, running_news_volume:=SMA(ifelse(is.na(news_items),0,news_items), n = 25 ),symbol]

#trying out strategies around insights types, but data only starts July '24
insights=just_news[, rbindlist(insights), by = .(publisher.name,date)] %>% unique
insights=merge(prices, 
               insights[,.(publisher.name,date,sentiment,symbol=ticker)],
               by=c('date','symbol'), all.y=T)

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
                    .(id, lead1sell_rally,lead1open,open,close, volume,market_cap,
                      avg_delta_short, avg_volume,day_drop_norm, lead1sell_rallydate,
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

news_moves[grepl('(dividend|repurchase|buyback)', title, ignore.case = T)  & 
             avg_delta_short<1 &
             !is.na(single_ticker) &
             publisher.name=='GlobeNewswire Inc.' &
             avg_volume>100000 & volume>100000 & close>5  & 
             market_cap <10*10^9  ][ #stock is boring
               order(day_drop_norm,decreasing = F),head(.SD,1),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol,
                   lead1sell_rallydate,hold_less_than = 5))


news_moves[lead1open/close >1.01 & 
             !is.na(single_ticker) &
             publisher.name %in% c('The Motley Fool','GlobeNewswire Inc.') &
             #grepl('(report|result|quarter)', title, ignore.case = T)  & 
             avg_volume>100000 & volume>100000 & close>7    ][ #stock is boring
               order(close,decreasing = F),head(.SD,1),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol,
                   lead1sell_rallydate,hold_less_than = 5))


news=fread("https://huggingface.co/datasets/Zihan1004/FNSPID/resolve/main/Stock_news/All_external.csv?download=true")
news[,domain:=stringr::str_extract(Url, "(?<=(http(s)?://)?(www)?)([a-zA-Z0-9-]+(\\.[a-zA-Z0-9-]+)+)(?=(/|:|$))")]
domains = unique(news$domain)
for(i in 1:length(domains)){
  fwrite(news[domain==domains[i]], paste("~/datasets/news_",domains[i],".csv", collapse =""))
}
#restart R
zacks_news=fread("~/datasets/news_ www.zacks.com .csv")
zacks_news=zacks_news[Url %in% zacks_news[,.N,Url][N==1,Url],.(date=as.IDate(Date),symbol=Stock_symbol, title=Article_title)]
zacks_news=merge(prices,zacks_news, by=c('date','symbol'))
zacks_news[grepl('earning', title, ignore.case = T) &
             grepl('revenue', title, ignore.case = T) &
             avg_volume>50000 & volume>50000 & close>5 & 
             (open-close > avg_range/2 )][order(day_drop_norm),head(.SD,1),date]%>%
  with(performance(date,lead1sell_rally/lead1open-1,lead1sell_rallydate-date,symbol,
                   lead1sell_rallydate,hold_less_than = 5))


news[domain=='www.zacks.com'][grepl('earning', Article_title, ignore.case = T) &
                                grepl('revenue', Article_title, ignore.case = T)][
                                  mapply(function(symbol, title) {
                                    pattern <- paste0("\\(", symbol, "\\)") # Escape parentheses
                                    grepl(pattern, title, ignore.case = TRUE)
                                  }, Stock_symbol, Article_title)
                                ][order(Date)]
