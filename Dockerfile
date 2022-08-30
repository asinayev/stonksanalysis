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
    git checkout de49483d53fb4f6eebaa7f4d2ce37038bb987b29 
 
CMD bash /home/repos/stonksanalysis/launch.sh
