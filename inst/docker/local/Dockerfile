FROM rocker/geospatial:3.4.4
LABEL maintainer="o2r project <http://o2r.info>"

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libapparmor-dev

RUN ["install2.r", "futile.logger"]

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

# use devtools instead of installGithub.r to take advantage of "Remotes" defined in DESCRIPTION file, and run vanilla so the above options are not used yet
# FIXME devtools somehow does not pick up on the remote for harbor, so install it first manually
RUN ["R", "--vanilla", "-e", "devtools::install_github('wch/harbor')"]
RUN ["R", "--vanilla", "-e", "devtools::install()"]

CMD ["R"]
