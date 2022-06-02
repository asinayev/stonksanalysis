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
    git checkout 9f5cef76fd4c137eaec30f1f1f1862f689c3b869 
 
CMD bash /home/repos/stonksanalysis/launch.sh
