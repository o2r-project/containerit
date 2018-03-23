FROM rocker/r-ver:3.3.2
LABEL maintainer="matthiashinz"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y git-core \
	libapparmor-dev \
	NULL
RUN ["install2.r", "assertthat", "backports", "clisymbols", "colorspace", "crayon", "debugme", "desc", "futile.logger", "futile.options", "jsonlite", "lambda.r", "magrittr", "munsell", "plyr", "R6", "Rcpp", "remotes", "rprojroot", "rstudioapi", "scales", "semver", "sessioninfo", "stringi", "stringr", "sys", "withr", "yaml"]
RUN ["installGithub.r", "r-hub/sysreqs@e4050e6068655ce519bb39f0508c7f10e19b6f0b", "wch/harbor@4e6ce36dee3571f95a6c8ee7010e298e94bcd976"]
WORKDIR /payload/
CMD ["R"]
