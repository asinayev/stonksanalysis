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
    git checkout 0bbf1ae5d84e73b60f2cb500b5e84524487f45e7 
 
CMD bash /home/repos/stonksanalysis/launch.sh
