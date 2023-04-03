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
    git checkout dae7944b7903af8f2abd964f5f29d684a1822ec2 
 
CMD bash /home/repos/stonksanalysis/launch.sh
