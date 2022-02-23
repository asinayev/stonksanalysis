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
    git checkout bd3faffd959f4111b1a5d2ae5a8c5c17b80e4cea 
 
CMD bash /home/repos/stonksanalysis/launch.sh
