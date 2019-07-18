FROM rocker/r-ver:3.4.4
LABEL maintainer="o2r"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y git-core \
	pandoc \
	pandoc-citeproc
RUN ["install2.r", "abe", "boxoffice", "digest", "evaluate", "htmltools", "knitr", "magrittr", "Rcpp", "rmarkdown", "stringi", "stringr", "xfun", "yaml"]
WORKDIR /payload/
CMD ["R"]
