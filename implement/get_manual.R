args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  setwd('~/stonksanalysis')
} else {
  setwd(args[1]) 
}
source("implement/imports.R", local=T)

install.packages("googlesheets4")
library(googlesheets4)

read_sheet("https://docs.google.com/spreadsheets/d/1bSW8k45QPlnL1aIijoRvRdZYhfKvHoz_TCWVGAZdGaE",
           sheet = 'exceptions') %>%
  write_strat(strat_name='exceptions', base_dir='/tmp/stonksanalysis')

read_sheet("https://docs.google.com/spreadsheets/d/1bSW8k45QPlnL1aIijoRvRdZYhfKvHoz_TCWVGAZdGaE",
           sheet = 'manual_buys') %>%
  write_strat(strat_name='manual', base_dir='/tmp/stonksanalysis')
