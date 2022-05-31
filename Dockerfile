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
    git checkout cd5be2ff3c6a80d6dac92d8a9970677f1d908e38 
 
CMD bash /home/repos/stonksanalysis/launch.sh
