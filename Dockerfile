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
    git checkout b02eb6c2851c3e62f97db6d7b57f8b75be1de497 
 
CMD bash /home/repos/stonksanalysis/launch.sh
