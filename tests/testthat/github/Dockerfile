FROM rocker/r-ver:3.3.2
LABEL maintainer="o2r"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y git-core
RUN ["install2.r", "assertthat", "backports", "crayon", "debugme", "desc", "R6", "remotes", "rprojroot"]
RUN ["installGithub.r", "r-hub/sysreqs@e4050e6068655ce519bb39f0508c7f10e19b6f0b"]
WORKDIR /payload/
CMD ["R"]
