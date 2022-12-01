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
    git checkout f75bc1a30ba4e56fbccc861fa8b696129e55e929 
 
CMD bash /home/repos/stonksanalysis/launch.sh
