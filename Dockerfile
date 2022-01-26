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
    git checkout c9716ca399cdd24e9805a720c3f8ebbf2589b983 
 
CMD bash /home/repos/stonksanalysis/launch.sh
