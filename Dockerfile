FROM rocker/rstudio:latest

# Install OpenJDK-8
RUN apt-get update && \
    apt-get install -y openjdk-8-jdk && \
    apt-get install -y ant && \
    apt-get clean;
    
# Fix certificate issues
RUN apt-get update && \
    apt-get install ca-certificates-java && \
    apt-get clean && \
    update-ca-certificates -f;

RUN install2.r --error \
    --deps TRUE \
    data.table \
    readxl \
    tidyquant \ 
    rpart.plot
    
