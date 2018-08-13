FROM rocker/r-ver:3.4.3
LABEL maintainer="o2r"
RUN ["install2.r", "versions"]
RUN ["Rscript", "-e", "versions::install.versions('cowsay', '0.5.0')", "-e", "versions::install.versions('fortunes', '1.5-3')"]
WORKDIR /payload/
CMD ["R"]
