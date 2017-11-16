FROM rocker/r-ver:3.3.2
LABEL maintainer="matthiashinz"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
 && apt-get install -y libapparmor-dev
RUN ["install2.r", "Rcpp", "crayon", "rprojroot", "assertthat", "plyr", "R6", "sys", "jsonlite", "backports", "scales", "debugme", "desc", "munsell", "colorspace", "remotes"]
RUN ["installGithub.r", "r-hub/sysreqs@481d263", "wch/harbor@4e6ce36"]
WORKDIR /payload/
CMD ["R"]
