require(tidyquant)
require(data.table)

fundamentals = fread("stonksanalysis/other_datasources/nasdaq_screener_1638810601422.csv") # from https://www.nasdaq.com/market-activity/stocks/screener
fundamentals = fundamentals[sample(nrow(fundamentals))]

splits = 16
system.time(
  prices <- fundamentals$Symbol %>%
    unique %>% split(1:splits) %>%
    parallel::mclapply(tq_get,
                       from=Sys.Date()-2*365,
                       to=Sys.Date()+2,
                       mc.cores = splits
    ) %>%
    rbindlist(fill=T)
)
fwrite(prices,'/tmp/prices.csv')