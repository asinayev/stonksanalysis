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

RUN pip3 install --upgrade \
    google-api-python-client\ 
    google.genai\
    polygon-api-client\
    pandas\
    requests\
    beautifulsoup4\
    --break-system-packages

# Copy local files instead of cloning from git
COPY . /home/repos/stonksanalysis

CMD bash /home/repos/stonksanalysis/launch.sh
