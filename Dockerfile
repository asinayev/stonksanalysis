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
    git checkout 66917085bc6a7e1f8fe6dfd20b9fcd6dc4fc2022 
 
CMD bash /home/repos/stonksanalysis/launch.sh
