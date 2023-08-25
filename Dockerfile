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
    git checkout 52046a8f17a0cb2401938284cd10bd1bcf3ba33f 
 
CMD bash /home/repos/stonksanalysis/launch.sh
