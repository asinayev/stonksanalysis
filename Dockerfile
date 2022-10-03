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
    git checkout 050c7dbddbce90c60a6c98b1b2f4d7df13c4231e 
 
CMD bash /home/repos/stonksanalysis/launch.sh
