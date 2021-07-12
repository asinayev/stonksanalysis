FROM rocker/rstudio:latest

RUN install2.r --error \
    --deps TRUE \
    data.table \
    readxl \
    tidyquant

RUN git pull https://github.com/asinayev/stonksanalysis.git