FROM rocker/r-ver:3.4.0
LABEL maintainer="o2r"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y git-core \
	libudunits2-dev
RUN ["install2.r", "graphics", "grDevices", "grid", "Rcpp", "remotes", "units", "utils"]
RUN ["installGithub.r", "viking/r-yaml@v2.1.17"]
WORKDIR /payload/
CMD ["R"]
