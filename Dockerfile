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
    git checkout baf672bda95b32d8a10a2e37c64d32a7dc23d3d8 
 
CMD bash /home/repos/stonksanalysis/launch.sh
