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
    git checkout cad65e62f773ba3f3d112a1f7517f854a91f8202 
 
CMD bash /home/repos/stonksanalysis/launch.sh
