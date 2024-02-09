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
    git checkout 8cde9e6e3206fda2acf817d72e507d85a5747cc6 
 
CMD bash /home/repos/stonksanalysis/launch.sh
