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
    git checkout 9a742e1a1b512c1cfda30c5dbd3e02628a11858c 
 
CMD bash /home/repos/stonksanalysis/launch.sh
