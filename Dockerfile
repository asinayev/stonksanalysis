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
    git checkout 9c31d6e7b7a6d2ab0229af479e1b6ddb5c676d0c
    
CMD bash /home/repos/stonksanalysis/launch.sh
