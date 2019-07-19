# Dockerfile part of https://github.com/o2r-project/containerit/
FROM rocker/geospatial:3.5.3

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libapparmor-dev

RUN ["install2.r", "futile.logger"]

RUN ["R", "--vanilla", "-e", "devtools::install_github('richfitz/stevedore')"]

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

WORKDIR /containerit
COPY . /containerit

RUN ["R", "--vanilla", "-e", "devtools::install()"]

LABEL maintainer="o2r-project <https://o2r.info>" \
  org.label-schema.vendor="o2r project" \
  org.label-schema.url="http://o2r.info" \
  org.label-schema.name="containerit" \
  org.label-schema.description="Package R in Docker Containers" \
  org.label-schema.version=dev \
  org.label-schema.docker.schema-version="rc1"

CMD ["R"]
