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
    git checkout 34851bb2aabd19ca0360f331d3fd14a40bad97c1 
 
CMD bash /home/repos/stonksanalysis/launch.sh
