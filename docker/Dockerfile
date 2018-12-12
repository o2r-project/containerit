FROM rocker/r-ver:3.5.1
LABEL maintainer="pjs"
RUN ["install2.r", "-r https://bioconductor.org/packages/3.8/bioc -r https://bioconductor.org/packages/3.8/data/annotation -rhttps://bioconductor.org/packages/3.8/data/experiment -rhttps://bioconductor.org/packages/3.8/workflows", "BiocGenerics"]
RUN ["install2.r", "versions"]
RUN ["Rscript", "-e", "versions::install.versions('BBmisc', '1.11')", "-e", "versions::install.versions('BH', '1.66.0-1')", "-e", "versions::install.versions('zoo', '1.8-3')"]
WORKDIR /payload/
CMD ["R"]
