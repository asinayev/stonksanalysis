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
    git checkout 885b779 

CMD ["Rscript","/home/repos/stonksanalysis/implement_foreign/get_data.R"]
CMD ["Rscript","/home/repos/stonksanalysis/implement_foreign/regression_outs.R"]
CMD ["Rscript","/home/repos/stonksanalysis/implement_foreign/respond_overnight.R"]