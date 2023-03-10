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
    git checkout 69c1b7d92ead5ac3b7a52ad10e62473a78830f46 
 
CMD bash /home/repos/stonksanalysis/launch.sh
