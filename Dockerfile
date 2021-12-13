FROM rocker/rstudio:latest

WORKDIR /home/repos

RUN install2.r --error \
    --deps TRUE \
    data.table \
    readxl \
    tidyquant \ 
    rpart.plot
    
RUN git clone https://github.com/asinayev/stonksanalysis.git && \
    cd stonkanalysis && \
    git checkout 3a5a201bedfd2ed2392d7853a8fb47fff3cf8a93 

CMD ["Rscript","/home/repos/stonksanalysis/research/foreign_stocks.R"]
