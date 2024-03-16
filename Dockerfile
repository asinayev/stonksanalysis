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
    git checkout eec7145d6c4ac5e4f2706423a7eeca09b07068d4 
 
CMD bash /home/repos/stonksanalysis/launch.sh
