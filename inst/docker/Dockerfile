FROM rocker/verse:latest

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libapparmor-dev

# use devtools instead of installGithub.r to take advantage of "Remotes" defined in DESCRIPTION file
RUN ["R", "-e", "devtools::install_github('o2r-project/containerit')"]

RUN echo "library(containerit);" >> /usr/local/lib/R/etc/Rprofile.site

USER rstudio

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
