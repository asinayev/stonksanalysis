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
    git checkout 5aa2c9424246e0216f7bb4c3e6edef635e09c9fe 
 
CMD bash /home/repos/stonksanalysis/launch.sh
