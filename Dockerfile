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
    git checkout 56e47c33b93234e1bdbb80b24af6ff912379d6d3 
 
CMD bash /home/repos/stonksanalysis/launch.sh
