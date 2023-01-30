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
    git checkout e97a5d37086b5ac1f48b94287230a82b31f66db9 
 
CMD bash /home/repos/stonksanalysis/launch.sh
