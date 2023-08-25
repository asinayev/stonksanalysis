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
    git checkout ec7739de0d0a6a39f9591e15bc6322a03acfdd7f 
 
CMD bash /home/repos/stonksanalysis/launch.sh
