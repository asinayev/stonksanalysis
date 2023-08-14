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
    git checkout fb5b891b25b4ba3ad79b2810b60dc6f71ac7cc99 
 
CMD bash /home/repos/stonksanalysis/launch.sh
