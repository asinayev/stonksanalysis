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
    git checkout 319c064a0f73c95e4ba0475d70bc201cc92cda6c 
 
CMD bash /home/repos/stonksanalysis/launch.sh
