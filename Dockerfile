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
    git checkout bdfb516ff76b5813d3d3156dd420bf5c4552690f 
 
CMD bash /home/repos/stonksanalysis/launch.sh
