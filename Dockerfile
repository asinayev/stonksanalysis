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
    git checkout 26ba6275e3f2cde5d14aaa3c26a195069d5700b0 
 
CMD bash /home/repos/stonksanalysis/launch.sh
