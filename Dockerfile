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
    git checkout a3d910f70485adbce8c90f02c0da9d94d79893ec 
 
CMD bash /home/repos/stonksanalysis/launch.sh
