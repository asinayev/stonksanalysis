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
    git checkout a12bbb1da1f376c5f3bf5159fb7e21a3f2594ef8 
 
CMD bash /home/repos/stonksanalysis/launch.sh
