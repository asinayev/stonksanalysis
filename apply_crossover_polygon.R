# setwd("stonksanalysis")
POLYKEY = Sys.getenv('POLYGONKEY')
source("prep_data.R", local=T)
source("crossover_strategy.R", local=T)

inputs = expand.grid(short_range=c(7), mid_range=c(35,56), 
                     buy_trigger=c(0,.15), sell_trigger=c(0,-.15), 
                     deathcross=c(T,F), profit=c(-10,-.2,0,.2)
)
dates = seq(as.Date("2014-01-01"),as.Date("2020-01-01"),365)

chosenstockdatraw=sampled_data(POLYKEY, as.Date('2020-01-01'), nsample=500, 
                               exchange = c('XNYS','XNAS','XASE'))
chosenstockdatraw[,.N,stock][N<100] #Missing stocks

chosenstockdat = chosenstockdatraw %>% crossoverReturns(inputs[1,],dat = dates[1], summary_only = F)

calcReturnsInPeriod(chosenstockdat, 'test', transaction_fee=.01)[,.(absolute_profit = mean(absolute_profit),
                                                                    stocks=.N,
                                                                    trades = sum(trades) )]
calcReturnsInPeriod(chosenstockdat, 'test', transaction_fee=.01)[order(absolute_profit)]
chosenstockdat[Buy!=Sell]


proc_date = function(date, ins, nsample, key=POLYKEY){
  apidat = sampled_data(key, date, nsample, exchange = c('XNYS','XNAS','XASE'))
  outs = ins %>%
    apply(1, crossoverReturns, dat=apidat, date = date) %>%
    rbindlist %>%
    cbind(ins)
  outs$Date = date
  outs
  }


system.time({results = lapply(dates, proc_date, ins=inputs, nsample=500)})

proc_date(dates[1], ins=inputs, nsample=1000)
