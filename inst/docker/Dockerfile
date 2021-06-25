# Dockerfile part of https://github.com/o2r-project/containerit/
FROM rocker/verse:4.0.5

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libapparmor-dev

# use devtools instead of installGithub.r to take advantage of "Remotes" defined in DESCRIPTION file
RUN ["R", "-e", "devtools::install_github('o2r-project/containerit')"]

RUN echo 'old <- getOption("defaultPackages");\n \
options(defaultPackages = c(old, "containerit"));\n \
.First <- function() {\n \
  level <- Sys.getenv(x = "CONTAINERIT_FLOG_THRESHOLD", unset = "INFO", names = NA)\n \
  library(futile.logger)\n \
  futile.logger::flog.threshold(level, name = "containerit")\n \
}\n \
.Last <- function() {\n \
  if(!as.logical(Sys.getenv(x = "CONTAINERIT_SILENT", unset = "FALSE", names = NA))) {\n \
    cat("\n"); print(sessionInfo());\n \
    cat("\n"); print(options()$repos);\n \
  }\n \
}' >> /usr/local/lib/R/etc/Rprofile.site

USER rstudio

WORKDIR /containerit

ARG VERSION=dev
ARG VCS_URL
ARG VCS_REF
ARG BUILD_DATE
ARG META_VERSION

LABEL maintainer="o2r-project <https://o2r.info>" \
  org.label-schema.vendor="o2r project" \
  org.label-schema.url="https://o2r.info" \
  org.label-schema.name="containerit" \
  org.label-schema.description="Package R in Docker Containers" \
  org.label-schema.version=$VERSION \
  org.label-schema.vcs-url=$VCS_URL \
  org.label-schema.vcs-ref=$VCS_REF \
  org.label-schema.build-date=$BUILD_DATE \
  org.label-schema.docker.schema-version="rc1"

CMD ["R"]
