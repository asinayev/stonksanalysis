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
    git checkout 8905d5cffe2db91f4050352022eb94a6e26d5d48 
 
CMD bash /home/repos/stonksanalysis/launch.sh
