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
    git checkout d35360a94b6f1e1fe77dfebca9dea440adedc3bb 
 
CMD bash /home/repos/stonksanalysis/launch.sh
