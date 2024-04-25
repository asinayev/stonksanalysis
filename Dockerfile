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
    git checkout d8f01e0d2e60994ee9d7bbf87fdbdef86dbce187 
 
CMD bash /home/repos/stonksanalysis/launch.sh
