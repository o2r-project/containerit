FROM rocker/r-ver:3.3.2
LABEL maintainer="o2r"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y gdal-bin \
	libgdal-dev \
	libproj-dev
RUN ["install2.r", "FNN", "gstat", "intervals", "lattice", "rgdal", "sp", "spacetime", "xts", "zoo"]
WORKDIR /payload/
COPY ["package_script/gstat/zonal.R", "package_script/gstat/zonal.R"]
CMD ["R", "--vanilla", "-f", "package_script/gstat/zonal.R"]
