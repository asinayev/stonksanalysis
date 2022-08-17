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
    git checkout d3f5f7d009ce569b64e0990d0c6573b19bcb51d9 
 
CMD bash /home/repos/stonksanalysis/launch.sh
