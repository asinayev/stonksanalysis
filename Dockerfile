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
    git checkout 7cf05eeb233842a19c9fbb4a31b4894e10dc056b 

RUN apt-get update && apt-get install -y \
    python3-pip

RUN pip3 install --upgrade \
    google-api-python-client\ 
    google.genai\
    polygon-api-client\
    pandas\
    --break-system-packages

CMD bash /home/repos/stonksanalysis/launch.sh
