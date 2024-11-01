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
    git checkout b1044a224bfc865c2ee7698fa8f8656817d14086 

RUN apt-get update && apt-get install -y \
    python3-pip

#RUN pip3 install --upgrade \
#    google-api-python-client\ 
#    google-generativeai\
#    polygon-api-client

CMD bash /home/repos/stonksanalysis/launch.sh
