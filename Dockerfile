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
    git checkout 694ed34c27b911b4f31cfc8ad151a11fe4b5d05f 
 
CMD bash /home/repos/stonksanalysis/launch.sh
