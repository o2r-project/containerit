FROM rocker/r-ver:3.3.2
MAINTAINER "matthiashinz"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
 && apt-get install -y gdal-bin \
	libgdal-dev \
	libproj-dev
RUN ["install2.r", "-r 'https://cloud.r-project.org'", "gstat", "sp", "zoo", "rgdal", "xts", "spacetime", "FNN", "intervals", "lattice"]
WORKDIR /payload/
COPY ["test_script_gstat/zonal.R", "test_script_gstat/zonal.R"]
CMD ["R", "--vanilla", "-f", "test_script_gstat/zonal.R"]
