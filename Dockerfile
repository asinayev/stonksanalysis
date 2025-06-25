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
    git checkout 11ad0f08800cdf24267ade6a028046a4c071dc20 

RUN apt-get update && apt-get install -y \
    python3-pip

RUN pip3 install --upgrade \
    google-api-python-client\ 
    google-generativeai\
    polygon-api-client\
    pandas\
    --break-system-packages

CMD bash /home/repos/stonksanalysis/launch.sh
