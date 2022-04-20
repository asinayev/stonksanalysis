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
    git checkout 267915f229c49a3f47b98a4434f1f1f2f2007d1c 
 
CMD bash /home/repos/stonksanalysis/launch.sh
