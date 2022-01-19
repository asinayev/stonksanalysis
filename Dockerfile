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
    git checkout 754ed649f8b54f5239d6a0cb721fbe01f47a2a73 
 
CMD bash /home/repos/stonksanalysis/launch.sh
