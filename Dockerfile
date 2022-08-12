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
    git checkout f3e2768e0176bc5b45cee75b4fd04d7752fdfadb 
 
CMD bash /home/repos/stonksanalysis/launch.sh
