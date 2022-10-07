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
    git checkout 0f375873bc7d8b3bd27f7ecc5b1288511f505b63 
 
CMD bash /home/repos/stonksanalysis/launch.sh
