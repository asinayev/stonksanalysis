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
    git checkout d2f061b72895b7d5fddb8f48db1cbe78b57928cd 
 
CMD bash /home/repos/stonksanalysis/launch.sh
