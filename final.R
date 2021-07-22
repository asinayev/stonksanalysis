setwd("stonksanalysis")
source("prep_data.R", local=T)
source("crossover_strategy.R", local=T)

library(tidyquant)
library(sendmailR)

# From https://docs.google.com/spreadsheets/d/1XxQPrpZepdQteGU-LOzYAfPFDTU6E6vZwZfwgoxNVCA/edit#gid=0
topcompanies = c('AAPLD', 
'MSFTD',
'AMZND',
'GOOGD',
'GOOGLD',
'FBD',
'BRK.BD',
'BRK.AD',
'TSLAD',
'BABAD',
'TSMD',
'VD',
'NVDAD',
'JPMD',
'JNJD',
'WMTD',
'UNHD',
'MAD',
'PYPLD',
'HDD',
'PGD',
'BACD',
'DISD',
'ADBED',
'ASMLD',
'CMCSAD',
'NKED',
'ORCLD',
'XOMD',
'TMD',
'KOD',
'VZD',
'PFED',
'NFLXD',
'LLYD',
'CSCOD',
'INTCD',
'CRMD',
'PEPD',
'ABTD',
'ABBVD',
'TMOD',
'NVSD',
'DHRD',
'TD',
'ACND',
'NVOD',
'AVGOD',
'SHOPD',
'MRKD',
'CVXD',
'WFCD',
'UPSD',
'COSTD',
'TMUSD',
'TXND',
'MCDD',
'MSD',
'MDTD',
'BBLD',
'BHPD',
'SAPD',
'HOND',
'QCOMD',
'ULD',
'LIND',
'AZND',
'PMD',
'BMYD',
'NEED',
'SED',
'UNPD',
'RYD',
'RDS.BD',
'RDS.AD',
'AMGND',
'INTUD',
'SBUXD',
'CD',
'AXPD',
'LOWD',
'CHTRD',
'BLKD',
'RIOD',
'BUDD',
'SCHWD',
'RTXD',
'BAD',
'MRNAD',
'AMTD',
'SNYD',
'PDDD',
'GSD',
'TGTD',
'IBMD',
'PTRD',
'AMATD',
'BXD',
'SONYD',
'TDD')
topcompanies = substr(topcompanies,1,nchar(topcompanies)-1)

fulldat = topcompanies %>% 
  tq_get %>% data.table %>%
  basic_prep(
    rename_from=c("symbol","date","adjusted"),
    rename_to=c("stock","Date","AdjClose")
  )

x = data.table(short_range=3, mid_range=7, long_range=150, buy_trigger=-.25, 
               sell_trigger=.3,  profit=-10) %>% 
  crossoverReturns( dat=fulldat, summary = F, 
                    date = as.Date('2021-01-04'), 
                    end_date = Sys.Date()+1, 
                    transaction_fee=.0001)
x[Date==Sys.Date()-1][Buy|Own]

x[stock=='PDD'] %>% with(plot(Date, AdjCloseFilled, type='l'))
x[stock=='PDD'] %>% with(points(Date, mid_range_mean, type='l'))
x[stock=='PDD' & Buy!=Sell] %>% with(abline(v=Date))
x[stock=='PDD'] %>% View()
x[sample=='test',sum(Own>0),Date] %>% plot(type='l')
