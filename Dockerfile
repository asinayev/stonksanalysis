FROM rocker/rstudio:latest

RUN install2.r --error \
    --deps TRUE \
    data.table \
    readxl \
    tidyquant \ 
    rpart.plot
    
CMD git clone https://github.com/asinayev/stonksanalysis.git

CMD R ~/stonkanalysis/research/foreign_stocks.R
