install.packages("googledrive")
library("googledrive")

store_dt = function(dtable){
  localpath = paste0('~/',deparse(substitute(dtable)),'.csv')
  fwrite(dtable)
  drive_put(localpath)
}

get_dt = function(name){
  localpath=paste0('~/',name,'.csv')
  drive_download(paste0(name,'.csv'), localpath,overwrite = T )
  fread(localpath)
} 
