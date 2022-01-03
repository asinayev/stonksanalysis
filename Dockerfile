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
    git checkout b5ce84c70b6fe89298e3c6c2584a8ec420ba8d0d 

CMD bash /home/repos/stonksanalysis/launch.sh
