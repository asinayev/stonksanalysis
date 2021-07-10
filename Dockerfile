FROM rocker/rstudio:latest

RUN install2.r --error \
    --deps TRUE \
    data.table \
    dplyr \
    readxl \
    tidyquant
