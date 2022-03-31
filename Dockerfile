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
    git checkout 8ecf591e49b7c02eb08032c83b7623e6c2506e9a 
 
CMD bash /home/repos/stonksanalysis/launch.sh
