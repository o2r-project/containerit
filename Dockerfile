FROM rocker/rstudio
# docker build -t containerit .
# docker run --rm -p 8787:8787 -e PASSWORD=meatballs containerit
# open to localhost:8787, username rstudio and password set above
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y libcurl4-openssl-dev \
	libssl-dev \
	make
RUN ["install2.r", "assertthat", "backports", "crayon", "curl", "desc", "digest", "formatR", "fs", "futile.logger", "futile.options", "htmltools", "httpuv", "jsonlite", "lambda.r", "later", "magrittr", "mime", "miniUI", "pillar", "pkgconfig", "promises", "remotes", "R6", "Rcpp", "rlang", "rprojroot", "rstudioapi", "semver", "shiny", "shinyFiles", "stevedore", "stringi", "stringr", "tibble", "versions", "xtable"]
RUN apt-get update && apt-get install -y zlib1g-dev libxml2-dev
RUN Rscript -e "remotes::install_github('o2r-project/containerit')"
