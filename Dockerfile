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
    git checkout b72bcc9ca4a1bd1d2f049950c2ed53b6ea40013c 
 
CMD bash /home/repos/stonksanalysis/launch.sh
