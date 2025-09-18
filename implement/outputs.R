write_strat = function(strat_dat, strat_name, base_dir=out_dir, append=F, col.names=T,
                      blacklist=c('GOOG','GOOGL','FUTY','FBND','BAR','GGLS','GGLL','GOOW','GOOP','GOOY'), 
                      price_condition_min=FALSE, price_condition_max=FALSE){
  strat_dat[,symbol:=gsub("[^[:alnum:] ]", " ", symbol)]
  strat_dat = strat_dat[!symbol %in% blacklist]
  out_file_name=paste0(base_dir,"/",strat_name,".csv")
  if(! ('strike_price' %in% names(strat_dat))){
    strat_dat[,strike_price:=close]  
  }
  write_cols=c('symbol','action','order_type','strike_price','time_in_force')
  if(price_condition_min){
      write_cols=c(write_cols,'price_condition_min')
    }
  if(price_condition_max){
      write_cols=c(write_cols,'price_condition_max')
    }
  fwrite(strat_dat[,mget(write_cols)],
         out_file_name, append = append, col.names = col.names)
}
