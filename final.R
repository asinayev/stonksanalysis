setwd("stonksanalysis")
source("prep_data.R", local=T)
source("crossover_strategy.R", local=T)

library(tidyquant)

# From https://docs.google.com/spreadsheets/d/1XxQPrpZepdQteGU-LOzYAfPFDTU6E6vZwZfwgoxNVCA/edit#gid=0
topcompanies = c('AAPL','MSFT','AMZN','GOOGL','FB','BRK.B','JPM','JNJ','BABA','V','MA','WMT','BAC','INTC','PG','UNH','T','DIS','TSM','KO','VZ','XOM','NVS','CVX','CSCO','PEP','MRK','CMCSA','HD','PFE','NVDA','ORCL','WFC','BA','TM','ADBE','C','MCD','SAP','ABT','BHP','NVO','CRM','NFLX','HSBC','TSLA','BMY','PYPL','ABBV','NEE','PM','RDS.A','NKE','RTX','LLY','AMGN','MDT','ASML','COST','HON','AVGO','UNP','SNY','IBM','AZN','LMT','TMO','TXN','ACN','GSK','RY','DHR','AMT','CHTR','AXP','DEO','LIN','HDB','GE','TD','SBUX','QCOM','FIS','MMM','CVS','GILD','UPS','ENB','MDLZ','USB','FOX','SYK','GS','MO','CAT','ANTM','BTI','INTU','DUK','FISV','SO','ADP','MS','SPGI','BLK','CME','EL','PLD','TMUS','BUD','BDX','BKNG','ZTS','BNS','CB','CI','ISRG','NOW','CCI','MUFG','TFC','D','JD','PTR','CL','WBK','NOC','UL','COP','VRTX','SHOP','SNP','CSX','GPN','CNI','LFC','ITW','RIO','TAK','MMC','BIIB','BMO','AMD','BP','PDD','TJX','UBER','EQIX','BSX','AMA',
                 'VBR','VO','SPY',
                 'CNBS','YOLO','THCX',
                 'FVAL')

fulldat = topcompanies %>% 
  tq_get %>% data.table %>%
  basic_prep(
    rename_from=c("symbol","date","adjusted"),
    rename_to=c("stock","Date","AdjClose")
  )


x = data.table(short_range=14, mid_range=14, long_range=200, buy_trigger=c(0), sell_trigger=c(.3), 
               cooloff=150, sell_after=365, sell_last_day=F) %>% 
  crossoverReturns( dat=fulldat, summary = F, 
                    date = as.Date('2021-07-20'), 
                    end_date = Sys.Date()+1, 
                    transaction_fee=.0001)

x[Date == max(Date) & Own>0] # Should own
x[Date == max(Date) & CrossoverLong<0 & Own==0][order(CrossoverLong, decreasing=T), .(stock,long_range_mean)][1:10] # Maybe buy?
x[Date == max(Date) & Own>0][order(abs(pct_diff(LastBought,AdjClose,LastBought)), decreasing=T), # Maybe sell?
                             .(stock,AdjClose,LastBought, pct_diff(AdjClose, LastBought, LastBought))][1:10]


st = 'UNP'
x[stock==st] %>% with(plot(Date, AdjCloseFilled, type='l'))
x[stock==st] %>% with(points(Date, mid_range_mean, type='l'))
x[stock==st & Own] %>% with(points(Date, LastBought, type='p', col='blue'))
x[stock==st & BuySell<0] %>% with(abline(v=Date, col='blue'))
x[stock==st & BuySell>0] %>% with(abline(v=Date))
