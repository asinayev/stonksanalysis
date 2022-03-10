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
    git checkout dedc4e54c92247ff5ed52fcd36d38be1fa6f398a 
 
CMD bash /home/repos/stonksanalysis/launch.sh
