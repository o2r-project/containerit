FROM rocker/r-ver:3.3.0
LABEL maintainer="o2r"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y git-core \
	libudunits2-dev
RUN ["install2.r", "containerittest", "graphics", "grDevices", "grid", "Rcpp", "remotes", "units", "utils"]
RUN ["installGithub.r", "r-hub/sysreqs@master"]
WORKDIR /payload/
CMD ["R"]
