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
    git checkout db3acc629303f9d8e0bc1818bbf2b011200c3c23 
 
CMD bash /home/repos/stonksanalysis/launch.sh
