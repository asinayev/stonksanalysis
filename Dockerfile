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

RUN pip install --upgrade \
    google-api-python-client\ 
    .66e3b3cfaa1386668666d3b53f4afb43d7cabe0e genai\
    polygon-api-client\
    pandas\
    --break-system-packages

# Copy local files instead of cloning from git
COPY . /home/repos/stonksanalysis

CMD bash /home/repos/stonksanalysis/launch.sh
