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
    git checkout b0d5ccb7a80157f59c88159ce4986ff4750ca91c 
 
CMD bash /home/repos/stonksanalysis/launch.sh
