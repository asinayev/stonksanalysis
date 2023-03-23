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
    git checkout be197fd96cbd16ea7c61a45d8d401cac5f49b246 
 
CMD bash /home/repos/stonksanalysis/launch.sh
