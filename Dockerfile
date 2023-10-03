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
    git checkout dbcd8f741de2ea4bf59fb8a413724a99f90caaf1 
 
CMD bash /home/repos/stonksanalysis/launch.sh
