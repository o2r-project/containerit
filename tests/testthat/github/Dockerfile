FROM rocker/r-ver:3.3.2
LABEL maintainer="matthiashinz"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
 && apt-get install -y git-core \
	libapparmor-dev
RUN ["install2.r", "assertthat", "backports", "colorspace", "crayon", "debugme", "desc", "jsonlite", "munsell", "plyr", "R6", "Rcpp", "remotes", "rprojroot", "scales", "sys"]
RUN ["installGithub.r", "r-hub/sysreqs@e4050e6", "wch/harbor@4e6ce36"]
WORKDIR /payload/
CMD ["R"]
