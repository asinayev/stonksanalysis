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
    git checkout aaca43a4369cc036c818c16c02c03fea0de2ec74 
 
CMD bash /home/repos/stonksanalysis/launch.sh
