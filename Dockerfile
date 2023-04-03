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
    git checkout 5a0e569c6fe1a96d06f1081ee552f91ec6a344eb 
 
CMD bash /home/repos/stonksanalysis/launch.sh
