FROM rocker/geospatial:latest
LABEL maintainer="o2r project <http://o2r.info>"

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libapparmor-dev

RUN ["install2.r", "-r 'https://cloud.r-project.org'", "rjson", "futile.logger", "debugme"]

RUN ["installGithub.r", "r-hub/sysreqs", "o2r-project/containerit"]

RUN echo "library(containerit);" >> /usr/local/lib/R/etc/Rprofile.site

WORKDIR /containerit
CMD ["R"]
