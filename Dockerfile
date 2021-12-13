FROM rocker/rstudio:latest

RUN install2.r --error \
    --deps TRUE \
    data.table \
    readxl \
    tidyquant \ 
    rpart.plot
    
RUN git clone https://github.com/asinayev/stonksanalysis.git && \
    cd stonkanalysis && \
    git checkout 2187fcd78ae89da9e29419aada4aaf8d0876b057 

CMD ["Rscript","/root/stonksanalysis/research/foreign_stocks.R"]
