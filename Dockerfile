FROM rocker/r-ver:3.3.3
LABEL maintainer="daniel"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
 && apt-get install -y pandoc \
	pandoc-citeproc
RUN ["install2.r", "-r 'https://cloud.r-project.org'", "rjson", "magrittr", "futile.logger", "lambda.r", "futile.options", "stringi", "knitr", "stringr", "fortunes"]
WORKDIR /payload/
COPY ["./.RData", "./.RData"]
CMD ["R"]
