FROM rocker/r-ver:3.3.2
LABEL maintainer="matthiashinz"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
 && apt-get install -y git-core \
	libapparmor-dev
RUN ["install2.r", "assertthat", "backports", "clisymbols", "colorspace", "crayon", "debugme", "desc", "futile.logger", "futile.options", "jsonlite", "lambda.r", "magrittr", "munsell", "plyr", "R6", "Rcpp", "remotes", "rprojroot", "rstudioapi", "scales", "semver", "sessioninfo", "stringi", "stringr", "sys", "withr", "yaml"]
RUN ["installGithub.r", "r-hub/sysreqs@e4050e6", "wch/harbor@4e6ce36"]
WORKDIR /payload/
CMD ["R"]
