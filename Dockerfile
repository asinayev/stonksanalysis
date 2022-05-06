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
    git checkout 45f62a4fcf34c01e9991bb909fdca60d03aad49e 
 
CMD bash /home/repos/stonksanalysis/launch.sh
