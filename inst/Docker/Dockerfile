FROM rocker/r-ver:3.4
LABEL maintainer="o2r project <http://o2r.info>"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
 && apt-get install -y git-core \
	pandoc \
	pandoc-citeproc
RUN ["install2.r", "-r 'https://cloud.r-project.org'", "rmarkdown", "knitr"]
WORKDIR /containerit
CMD ["R", "--vanilla" ]
