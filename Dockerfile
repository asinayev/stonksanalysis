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
    git checkout 72f053e6b182959ca0c7e628655eb6eb279f9cbc 
 
CMD bash /home/repos/stonksanalysis/launch.sh
