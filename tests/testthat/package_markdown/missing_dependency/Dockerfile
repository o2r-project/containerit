FROM rocker/r-ver:3.4.4
LABEL maintainer="o2r"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y git-core \
	libcurl4-openssl-dev \
	libssl-dev \
	pandoc \
	pandoc-citeproc
RUN ["install2.r", "abe", "assertthat", "backports", "boxoffice", "crayon", "curl", "desc", "digest", "evaluate", "formatR", "futile.logger", "futile.options", "htmltools", "knitr", "lambda.r", "magrittr", "R6", "Rcpp", "rmarkdown", "rprojroot", "rstudioapi", "semver", "stevedore", "stringi", "stringr", "xfun", "yaml"]
WORKDIR /payload/
CMD ["R"]
