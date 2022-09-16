require(tidyquant, quietly = T)
require(data.table, quietly = T)

source("polygon.R", local=T)
source("implement/outputs.R", local=T)
source("implement/features.R", local=T)
POLYKEY = Sys.getenv('POLYGONKEY')
out_dir = Sys.getenv('out_dir')

stopifnot(POLYKEY!='')