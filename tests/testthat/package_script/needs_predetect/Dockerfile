FROM rocker/r-ver:3.4.4
LABEL maintainer="o2r"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y libcurl4-openssl-dev \
	libssl-dev \
	libxml2-dev
RUN ["install2.r", "boxoffice", "curl", "formatR", "futile.logger", "futile.options", "httr", "lambda.r", "magrittr", "R6", "Rcpp", "rvest", "selectr", "semver", "stevedore", "stringi", "stringr", "xml2"]
WORKDIR /payload/
CMD ["R"]
