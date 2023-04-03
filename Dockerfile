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
    git checkout 75ebd6b486937b307ceefe78d036e4c8e7d4235c 
 
CMD bash /home/repos/stonksanalysis/launch.sh
