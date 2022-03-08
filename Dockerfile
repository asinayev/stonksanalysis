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
    git checkout be09f00d67b6fefabdf5e32f536c40125b0dbe29 
 
CMD bash /home/repos/stonksanalysis/launch.sh
