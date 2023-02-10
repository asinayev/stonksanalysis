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
    git checkout 60a0bb52a1e67f1cb7e80de7b3b0dea15fbca756 
 
CMD bash /home/repos/stonksanalysis/launch.sh
