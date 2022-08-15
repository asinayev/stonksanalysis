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
    git checkout 3cf2b60d87bc877de62bd2a350a15a08a9e9ee08 
 
CMD bash /home/repos/stonksanalysis/launch.sh
