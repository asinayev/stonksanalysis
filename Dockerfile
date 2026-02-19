FROM rocker/rstudio:latest

WORKDIR /home/repos

RUN install2.r --error \
    --deps TRUE \
    data.table \
    readxl \
    tidyquant \ 
    rpart.plot
    
RUN apt-get update && apt-get install -y \
    python3-pip

RUN i9001757b38b4ff6ccad036730b8f821cd13e86b4 nstall --upgrade \
    google-api-python-client\ 
    google.genai\
    polygon-api-client\
    pandas\
    --break-system-packages

# Copy local files instead of cloning from git
COPY . /home/repos/stonksanalysis

CMD bash /home/repos/stonksanalysis/launch.sh
