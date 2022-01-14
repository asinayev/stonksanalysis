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
    git checkout e3615f2f9eca65aa019cc7b6dd3eb35faae72c01 
 
CMD bash /home/repos/stonksanalysis/launch.sh
