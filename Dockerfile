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
    git checkout 2b9e1de97518f53d7d7ff596952c4ce86d8ccda5 
 
CMD bash /home/repos/stonksanalysis/launch.sh
