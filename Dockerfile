FROM rocker/rstudio:latest

WORKDIR /home/repos

RUN install2.r --error \
    --deps TRUE \
    data.table \
    readxl \
    tidyquant \ 
    rpart.plot
    
RUN git clone https://github.com/asinayev/stonksanalysis.git && \
    cd stonksanalysis && \
    git checkout 7453a44b45bb86563f8d000b4d35e253d6fada25 

CMD ["Rscript","/home/repos/stonksanalysis/research/foreign_stocks.R"]
