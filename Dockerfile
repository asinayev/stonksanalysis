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
    git checkout 19c3751077a5dbe0632157cd6c7aadf83c16a3d5 
 
CMD bash /home/repos/stonksanalysis/launch.sh
