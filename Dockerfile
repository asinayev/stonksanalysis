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
    git checkout e28db0739664e1a4c8ac08f4dcda415aaa93c744 

RUN apt-get update && apt-get install -y \
    python3-pip

RUN pip3 install --upgrade \
    google-api-python-client\ 
    google-generativeai\
    polygon-api-client\
    pandas\
    --break-system-packages

CMD bash /home/repos/stonksanalysis/launch.sh
