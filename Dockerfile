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
    git checkout bcdd9bffffb38bb39f22a1f4aed7d44a014113ed 
 
CMD bash /home/repos/stonksanalysis/launch.sh
