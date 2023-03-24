write_strat = function(strat_dat, strat_name, base_dir=out_dir, append=F, col.names=T,
                      blacklist=c('GOOG','GOOGL','VTI','FUTY','VGLT','BAR','VGIT')){
  strat_dat[,symbol:=gsub("[^[:alnum:] ]", " ", symbol)]
  strat_dat = strat_dat[!symbol %in% blacklist]
  out_file_name=paste0(base_dir,"/",strat_name,".csv")
  fwrite(strat_dat,out_file_name, append = append, col.names = col.names)
}
