setwd("stonksanalysis")
source("prep_data.R", local=T)
source("crossover_strategy.R", local=T)
setDTthreads(threads = 2)

library(tidyquant)

# From https://docs.google.com/spreadsheets/d/1XxQPrpZepdQteGU-LOzYAfPFDTU6E6vZwZfwgoxNVCA/edit#gid=0
topcompanies = data.table(
  ticker = c('AAPL','MSFT','AMZN','GOOGL','FB','BRK','JPM','JNJ','BABA','V','MA','WMT','BAC','INTC','PG','UNH','T','DIS','TSM','KO','VZ','XOM','NVS','CVX','CSCO','PEP','MRK','CMCSA','HD','PFE','NVDA','ORCL','WFC','BA','TM','ADBE','C','MCD','SAP','ABT','BHP','NVO','CRM','NFLX','HSBC','TSLA','BMY','PYPL','ABBV','NEE','PM','RDS','NKE','RTX','LLY','AMGN','MDT','ASML','COST','HON','AVGO','UNP','SNY','IBM','AZN','LMT','TMO','TXN','ACN','GSK','RY','DHR','AMT','CHTR','AXP','DEO','LIN','HDB','GE','TD','SBUX','QCOM','FIS','MMM','CVS','GILD','UPS','ENB','MDLZ','USB','FOX','SYK','GS','MO','CAT','ANTM','BTI','INTU','DUK','FISV','SO','ADP','MS','SPGI','BLK','CME','EL','PLD','TMUS','BUD','BDX','BKNG','ZTS','BNS','CB','CI','ISRG','NOW','CCI','MUFG','TFC','D','JD','PTR','CL','WBK','NOC','UL','COP','VRTX','SHOP','SNP','CSX','GPN','CNI','LFC','ITW','RIO','TAK','MMC','BIIB','BMO','AMD','BP','PDD','TJX','UBER','EQIX','BSX','AMAT','AON','NSC','PNC','DE','GD','MON','WM','SCHW','TGT','SHW','RELX','TRP','VMW','ECL','APD','LOW','LVS','SLB','AEP','MU','HCA','EPD','GM','MCO','ICE','WBA','KMB','KMI','HUM','SMFG','EW','EXC','MET','ABB','LHX','COF','LYG','INFY','ING','PGR','NTES','HMC','ATVI','VOD','EMR','FDX','SPG','ILMN','RACE','REGN','AMOV','BAM','SU','MFC','ADI','AIG','UBS','SRE','PHG','BAX','ETN','BCE','KDP','BCS','PBR','DD','IBN','PSX','CNC','ALL','VALE','MAR','CM','WDAY','PSA','DAL','SYY','CTSH','PRU','ROP','NEM','AFL','XEL','ITUB','NGG','CP','MFG','MELI','BK','ZM','E','BX','TRI','DG','WELL','DOW','ABEV','TEF','TRV'),
  ticker_valid_start=as.Date('2021-08-05'), ticker_valid_end=as.Date('2021-08-05')+365
  
)

fulldat = topcompanies$ticker %>% 
  tq_get %>% data.table %>%
  basic_prep(
    rename_from=c("symbol","date","adjusted"),
    rename_to=c("stock","Date","AdjClose")
  )%>%
  filter_range(topcompanies)


params = data.table(short_range=c(50,50,28), long_range=c(250),
               buy_trigger=c(-.15,-.1,-.05), cooloff=c(0), buy_trigger_days_max = c(50,100,150), buy_trigger_days_min = c(0,14,70),
               buy_atr_min=c(0.02), buy_rsi_max=c(.5), sell_rsi_min=c(1.1),
               sell_hi=c(.15), sell_lo=c(.25), sell_atr = c(100),
               sell_days=c(180), sell_last=c(T))

thisparam=params[2,]
x = crossoverReturns( thisparam, dat=fulldat, summary = F, transaction_fee=.0001)

x[Date == max(Date) & Own>0] # Should own

x[Date == max(Date) & Own>0][order(abs(pct_diff(LastBought,AdjClose,LastBought)), decreasing=T), # Maybe sell?
                             .(stock,AdjClose,LastBought, pct_diff(AdjClose, LastBought, LastBought))][1:10]

x[Date==max(Date)& CrossoverLong<thisparam$buy_trigger+.01 & 
    CrossoverLong>thisparam$buy_trigger-.1 & 
    Own==0, .(Date, stock, rel_ati = atr/AdjClose, rsi, CrossoverLong)]

for (st in
x[Date == max(Date) & 
    CrossoverLong<thisparam$buy_trigger+.01 & 
    CrossoverLong>thisparam$buy_trigger-.1 & 
    Own==0, stock  ]){
  plot(x=Sys.Date()-(1:(thisparam$buy_trigger_days_max+50)), 
       y=seq(-.2,.2, length.out=thisparam$buy_trigger_days_max+50), col='white',
       main = st)
  abline(h=thisparam$buy_trigger)
  abline(v=Sys.Date()-thisparam$buy_trigger_days_min)
  abline(v=Sys.Date()-thisparam$buy_trigger_days_max)

  x[stock==st & CrossoverLong] %>% with(points(Date,CrossoverLong, type='l'))
  print(x[Date==max(Date) & stock==st, .(Date, stock, atr/AdjClose, rsi)])
  Sys.sleep(3)
}
