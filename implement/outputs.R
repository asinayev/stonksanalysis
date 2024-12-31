write_strat = function(strat_dat, strat_name, base_dir=out_dir, append=F, col.names=T,
                      blacklist=c('GOOG','GOOGL','FUTY','FBND','BAR')){
  strat_dat[,symbol:=gsub("[^[:alnum:] ]", " ", symbol)]
  strat_dat = strat_dat[!symbol %in% blacklist]
  out_file_name=paste0(base_dir,"/",strat_name,".csv")
  if(! ('strike_price' %in% names(strat_dat))){
    strat_dat[,strike_price:=close]  
  }
  fwrite(strat_dat[,.(symbol,action,order_type,strike_price,time_in_force)],
         out_file_name, append = append, col.names = col.names)
}
