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
    git checkout c3de19e5b90ae5bd8dcfc5399a49ab0362dbf87f 
 
CMD bash /home/repos/stonksanalysis/launch.sh
