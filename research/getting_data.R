sampled_data_yr = function(yr, ...)(
  sampled_data(..., 
               date=paste(yr,'01','01',sep='-'),
               end_date=paste(yr,'12','31',sep='-')
               )%>%
    fwrite(paste0("/home/rstudio/datasets/stocks_by_yr/",yr,".csv.gz"))
)

lapply(2004:2025, sampled_data_yr, key=POLYKEY, ticker_type='', market='stocks', details=T, get_unadjusted=T)

sampled_data(key=POLYKEY, ticker_type='', market='stocks', details=T, 
              get_unadjusted=T, date='2008-10-01', end_date = '2009-12-31') %>%
   subset(year(date)==2009) %>%
   fwrite(paste0("/home/rstudio/datasets/stocks_by_yr/2009.csv.gz"))
