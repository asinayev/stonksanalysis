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
    git checkout 233c0afd7194c41c9c057e2ebc3ba5cf2335a96b 
 
CMD bash /home/repos/stonksanalysis/launch.sh
