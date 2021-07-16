install.packages(c("devtools"))
devtools::install_github("ldurazo/kaggler")
library(kaggler)
library(stringr)

# save your kaggle credentials from https://www.kaggle.com/USERNAME/account
kgl_auth(creds_file = 'kaggle.json')

response <- kgl_datasets_download_all(owner_dataset = "jacksoncrow/stock-market-dataset")
download.file(response[["url"]], "data/temp.zip", mode="wb")
unzip_list <- unzip("data/temp.zip", exdir = "data/", list=TRUE )
stock_filenames <- unzip_list$Name[str_detect(unzip_list$Name, "stocks")]
unzip_result <- unzip("data/temp.zip", exdir = "data/", overwrite = TRUE, files=stock_filenames )
