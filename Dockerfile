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
    git checkout dcde1871f3b7a392874f4a78495d2287c8723ee7 
 
CMD bash /home/repos/stonksanalysis/launch.sh
