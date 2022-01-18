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
    git checkout cf5b0afb7dc80a80aa116a476f43448c4ee3d455 
 
CMD bash /home/repos/stonksanalysis/launch.sh
