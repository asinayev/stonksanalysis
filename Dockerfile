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
    git checkout 85e46ed5a8bb43e5c6af63407bdbe31cc2d383fb 
 
CMD bash /home/repos/stonksanalysis/launch.sh
