setwd("stonksanalysis")
source("prep_data.R", local=T)
source("crossover_strategy.R", local=T)

library(tidyquant)

# From https://docs.google.com/spreadsheets/d/1XxQPrpZepdQteGU-LOzYAfPFDTU6E6vZwZfwgoxNVCA/edit#gid=0
topcompanies = data.table(
  ticker = c('AAPL','MSFT','AMZN','GOOGL','FB','BRK','JPM','JNJ','BABA','V','MA','WMT','BAC','INTC','PG','UNH','T','DIS','TSM','KO','VZ','XOM','NVS','CVX','CSCO','PEP','MRK','CMCSA','HD','PFE','NVDA','ORCL','WFC','BA','TM','ADBE','C','MCD','SAP','ABT','BHP','NVO','CRM','NFLX','HSBC','TSLA','BMY','PYPL','ABBV','NEE','PM','RDS','NKE','RTX','LLY','AMGN','MDT','ASML','COST','HON','AVGO','UNP','SNY','IBM','AZN','LMT','TMO','TXN','ACN','GSK','RY','DHR','AMT','CHTR','AXP','DEO','LIN','HDB','GE','TD','SBUX','QCOM','FIS','MMM','CVS','GILD','UPS','ENB','MDLZ','USB','FOX','SYK','GS','MO','CAT','ANTM','BTI','INTU','DUK','FISV','SO','ADP','MS','SPGI','BLK','CME','EL','PLD','TMUS','BUD','BDX','BKNG','ZTS','BNS','CB','CI','ISRG','NOW','CCI','MUFG','TFC','D','JD','PTR','CL','WBK','NOC','UL','COP','VRTX','SHOP','SNP','CSX','GPN','CNI','LFC','ITW','RIO','TAK','MMC','BIIB','BMO','AMD','BP','PDD','TJX','UBER','EQIX','BSX','AMAT','AON','NSC','PNC','DE','GD','MON','WM','SCHW','TGT','SHW','RELX','TRP','VMW','ECL','APD','LOW','LVS','SLB','AEP','MU','HCA','EPD','GM','MCO','ICE','WBA','KMB','KMI','HUM','SMFG','EW','EXC','MET','ABB','LHX','COF','LYG','INFY','ING','PGR','NTES','HMC','ATVI','VOD','EMR','FDX','SPG','ILMN','RACE','REGN','AMOV','BAM','SU','MFC','ADI','AIG','UBS','SRE','PHG','BAX','ETN','BCE','KDP','BCS','PBR','DD','IBN','PSX','CNC','ALL','VALE','MAR','CM','WDAY','PSA','DAL','SYY','CTSH','PRU','ROP','NEM','AFL','XEL','ITUB','NGG','CP','MFG','MELI','BK','ZM','E','BX','TRI','DG','WELL','DOW','ABEV','TEF','TRV'),
  ticker_valid_start=as.Date('2021-08-05'), ticker_valid_end=as.Date('2021-08-09')+365
  
)

fulldat = topcompanies$ticker %>% 
  tq_get %>% data.table %>%
  basic_prep(
    rename_from=c("symbol","date","adjusted"),
    rename_to=c("stock","Date","AdjClose")
  )%>%
  filter_range(topcompanies)


x = data.table(short_range=c(75), long_range=c(300),
               buy_trigger=c(-.1), cooloff=c(0), buy_trigger_days_max = c(100), buy_trigger_days_min = c(28),
               sell_hi=c(.225), sell_lo=c(.275), sell_atr = c(100),
               sell_days=c(365), sell_last=c(F)) %>% 
  crossoverReturns( dat=fulldat, summary = F, transaction_fee=.0001)

x[Date == max(Date) & Own>0] # Should own
x[Date == max(Date) & CrossoverLong< -.1 & Own==0][order(CrossoverLong, decreasing=T), .(stock,long_range_mean)][1:10] # Maybe buy?
x[Date == max(Date) & Own>0][order(abs(pct_diff(LastBought,AdjClose,LastBought)), decreasing=T), # Maybe sell?
                             .(stock,AdjClose,LastBought, pct_diff(AdjClose, LastBought, LastBought))][1:10]


st = 'BABA'
x[stock==st & CrossoverLong< -.1]
x[stock==st] %>% with(plot(Date, AdjCloseFilled, type='l'))
x[stock==st] %>% with(points(Date, mid_range_mean, type='l'))
x[stock==st & Own] %>% with(points(Date, LastBought, type='p', col='blue'))
x[stock==st & BuySell<0] %>% with(abline(v=Date, col='blue'))
x[stock==st & BuySell>0] %>% with(abline(v=Date))
