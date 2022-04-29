install.packages("googlesheets4")
library(googlesheets4)

read_sheet("https://docs.google.com/spreadsheets/d/1bSW8k45QPlnL1aIijoRvRdZYhfKvHoz_TCWVGAZdGaE",
           sheet = 'exceptions') %>%
  fwrite('/tmp/stonksanalysis/exceptions.csv')

read_sheet("https://docs.google.com/spreadsheets/d/1bSW8k45QPlnL1aIijoRvRdZYhfKvHoz_TCWVGAZdGaE",
           sheet = 'manual_buys') %>%
  fwrite('/tmp/stonksanalysis/manual.csv')
