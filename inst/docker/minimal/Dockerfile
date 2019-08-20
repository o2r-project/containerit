FROM rocker/r-base:latest

RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y libcurl4-openssl-dev \
	libssl-dev \
	libxml2-dev \
	make

RUN ["install2.r", "remotes"]

#RUN apt-get update && apt-get install -y zlib1g-dev
RUN Rscript -e "remotes::install_github('o2r-project/containerit')"

## Build and run image:
# docker build -t containerit-minimal .
# docker run --rm -it containerit-minimal
## In R:
# library("containerit")
# print(dockerfile())
