FROM rocker/r-ver:latest
LABEL maintainer="o2r project <http://o2r.info>"

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libapparmor-dev

RUN ["install2.r", "-r 'https://cloud.r-project.org'", "rjson", "futile.logger", "debugme"]

RUN ["installGithub.r", "r-hub/sysreqs", "o2r-project/containerit"]

RUN echo "library(containerit);" >> /usr/local/lib/R/etc/Rprofile.site

RUN useradd o2r
USER o2r

WORKDIR /containerit

ARG VERSION=dev
ARG VCS_URL
ARG VCS_REF
ARG BUILD_DATE
ARG META_VERSION

LABEL maintainer="o2r-project <https://o2r.info>" \
  org.label-schema.vendor="o2r project" \
  org.label-schema.url="http://o2r.info" \
  org.label-schema.name="containerit" \
  org.label-schema.description="Package R in Docker Containers" \
  org.label-schema.version=$VERSION \
  org.label-schema.vcs-url=$VCS_URL \
  org.label-schema.vcs-ref=$VCS_REF \
  org.label-schema.build-date=$BUILD_DATE \
  org.label-schema.docker.schema-version="rc1"

CMD ["R"]
