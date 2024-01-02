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
    git checkout f53b3d0f7a0eac8b892bce1e391184bf916b9375 
 
CMD bash /home/repos/stonksanalysis/launch.sh
