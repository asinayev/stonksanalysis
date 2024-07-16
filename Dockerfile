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
    git checkout 9e6e6cb8a38ce630dad43adc3d6959b106f8a8d8 
 
CMD bash /home/repos/stonksanalysis/launch.sh
