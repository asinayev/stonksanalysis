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
    git checkout 3c04ea9b06b7ed4082f5ac295ba46eac5d5047f2 
 
CMD bash /home/repos/stonksanalysis/launch.sh
