FROM rocker/r-ver:3.3.2
LABEL maintainer="matthiashinz"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
 && apt-get install -y gdal-bin \
	libcurl4-openssl-dev \
	libgdal-dev \
	libgeos-dev \
	libproj-dev \
	libudunits2-dev \
	libv8-3.14-dev \
	make \
	wget
WORKDIR /tmp/gdal
RUN wget http://download.osgeo.org/gdal/2.1.3/gdal-2.1.3.tar.gz \
 && tar zxf gdal-2.1.3.tar.gz \
 && cd gdal-2.1.3 \
 && ./configure \
 && make \
 && make install \
 && ldconfig \
 && rm -r /tmp/gdal
RUN ["install2.r", "Rcpp", "lattice", "V8", "R6", "jsonlite", "DBI", "magrittr", "units", "httr", "curl", "sp", "rgdal", "udunits2", "foreign", "maptools", "rgeos", "remotes"]
RUN ["installGithub.r", "ropensci/geojsonio@2e15e21", "edzer/sfr@7057e42"]
WORKDIR /payload/
CMD ["R"]
