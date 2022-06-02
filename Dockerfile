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
    git checkout 6ca63b3489c85a7ec2e74802e8f2e4dcdd8d0e52 
 
CMD bash /home/repos/stonksanalysis/launch.sh
