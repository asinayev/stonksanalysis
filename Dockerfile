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
    git checkout fe72a86f8545d4cbf257a74704f46c41dd5ae475 
 
CMD bash /home/repos/stonksanalysis/launch.sh
