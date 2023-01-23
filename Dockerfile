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
    git checkout 62ffd36a950b4d0b08cf9b5db1a633b65f694f27 
 
CMD bash /home/repos/stonksanalysis/launch.sh
