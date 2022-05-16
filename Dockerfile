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
    git checkout 67d743ef7d50c8f3ab2ed494261e6158a3e51f6a 
 
CMD bash /home/repos/stonksanalysis/launch.sh
