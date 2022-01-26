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
    git checkout d218b7c32790a49e7bb26fcc4db395bdc6e5b511 
 
CMD bash /home/repos/stonksanalysis/launch.sh
