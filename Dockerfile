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
    git checkout d0e6f7cb0df70d7386e11c3e4b9075311f15dd85 
 
CMD bash /home/repos/stonksanalysis/launch.sh
