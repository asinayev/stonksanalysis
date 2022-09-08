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
    git checkout 4b8b93145ae76259e74aeb1b9070399a30f23252 
 
CMD bash /home/repos/stonksanalysis/launch.sh
