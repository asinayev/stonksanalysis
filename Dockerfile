FROM rocker/rstudio:latest

RUN install2.r --error \
    --deps TRUE \
    data.table \
    readxl \
    tidyquant \ 
    rpart.plot
    
RUN apt-get install -y default-jre

RUN apt-get install -y default-jdk

RUN git clone https://github.com/asinayev/stonksanalysis.git

COPY stonkanalysis /stonkanalysis
