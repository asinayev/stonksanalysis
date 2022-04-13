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
    git checkout 4b2188caff9039d91f20b0d6eaeacc2eafa3cec4 
 
CMD bash /home/repos/stonksanalysis/launch.sh
