FROM rocker/r-ver:3.3.2
LABEL maintainer="o2r"
RUN ["install2.r", "-r https://bioconductor.org/packages/3.9/bioc -r https://bioconductor.org/packages/3.9/data/annotation -r https://bioconductor.org/packages/3.9/data/experiment -r https://bioconductor.org/packages/3.9/workflows", "BiocGenerics"]
WORKDIR /payload/
CMD ["R"]
