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
    git checkout 9abd3d7a859a5ce33ba1da7f18fe6815477d0b89 
 
CMD bash /home/repos/stonksanalysis/launch.sh
