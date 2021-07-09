source("prep_data.R", local=T)
source("trade_strategy.R", local=T)

install.packages("readxl")
library(tidyquant)
library(TTR)

train_start = "2019-02-01"
train_end = "2020-02-01"
test_start = "2020-05-01"
test_end = "2021-05-01"
oos_start = "2021-05-01"
today = "2021-07-09"
today = "2021-07-10"

# chosenstocks = c("CIX"   ,"GBR"   ,"GRF"   ,"GSS"   ,"FAS"   ,"IYF"   ,"JHMF"  ,"RIGS"  ,"TOUR"  ,"ZIONP",
#                  "IBMJ"  ,"JPN"   ,"JPXN"  ,"MINC"  ,"MUNI"  ,"MUST"  ,"PGHY"  ,"PUTW"  ,"RIGS"  ,"SCJ", 
#                  "UCON"  ,"AFIF"  ,"BBJP"  ,"CEMB"  ,"EMTL"  ,"FLBL"  ,"IBHA"  ,"JMST"  ,"JMUB"  ,"MEAR"  ,
#                  "PJUL"  , "SMB"  ,"VAMO"  ,"XMPT"  ,"ASML"  ,"ATNX"  ,"BFST"  ,"BPOPM" ,"BPOPN" ,"BPYUP", 
#                  "CBAN"  ,"CLRG"  ,"COWNL" ,"CYAN"  ,"EDSA"  ,"EMCB"  ,"FCAL"  ,"FMB"   ,"FTSL"  ,"HBANN" ,
#                  "HBANO" ,"IRMD" )

chosenstocks = sample(TTR::stockSymbols()$Symbol,1500)

system.time({chosenstockdat = tq_get(chosenstocks, from = train_start, to = tomorrow) %>% data.table}) #takes about a minute per 100 stonks

stockdatPrepped = prep_data(chosenstockdat, 
                            train_start = train_start,
                            train_end = train_end,
                            oos_start = oos_start,
                            oos_end = today,
                            test_start = test_start,
                            test_end = test_end,
                            rename_from=c("symbol","date","adjusted"),rename_to=c("stock","Date","AdjClose"),
                            future=F)
results = strategy(params=c("minPerformance"=.02, "minAlpha"=.008), stockdatPrepped, chosenstocks)
results[error_improvement>.02 & PredDiff>.008 & training_samples>100, 
        .(sum(CloseDiff),mean(CloseDiff),.N)]
results[,.(.N,sum(CloseDiff)),Date][order(Date)]
results[,.(.N,sum(CloseDiff)),stock][order(stock)]

stockdatPrepped = prep_data(chosenstockdat, 
          train_start = train_start,
          train_end = train_end,
          test_start = test_start,
          test_end = test_end,
          oos_start = today,
          oos_end = tomorrow,
          rename_from=c("symbol","date","adjusted"),rename_to=c("stock","Date","AdjClose"),
          future=T
          )
strategy(params=c("minPerformance"=.02, "minAlpha"=.008), stockdatPrepped, chosenstocks, future=T)
