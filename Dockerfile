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
    git checkout 4b669cef9bb27ade25c2bfe56f231a036ed8d7a7 
 
CMD bash /home/repos/stonksanalysis/launch.sh
