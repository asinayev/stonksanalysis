FROM rocker/rstudio:latest

RUN install2.r --error \
    --deps TRUE \
    data.table \
    readxl \
    tidyquant \ 
    rpart.plot
    
CMD ["git", "clone", "https://github.com/asinayev/stonksanalysis.git", "/repos/"]

CMD ["Rscript","/repos/stonkanalysis/research/foreign_stocks.R"]
