FROM rocker/rstudio:latest

RUN install2.r --error \
    --deps TRUE \
    data.table \
    readxl \
    tidyquant \ 
    rpart.plot
    
RUN git clone https://github.com/asinayev/stonksanalysis.git ~/stonks/

CMD ["Rscript","/root/stonks/research/foreign_stocks.R"]
