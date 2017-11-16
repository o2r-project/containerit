FROM rocker/r-ver:3.3.2
LABEL maintainer="matthiashinz"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
 && apt-get install -y gdal-bin \
	libgdal-dev \
	libproj-dev
RUN ["install2.r", "gstat", "sp", "zoo", "rgdal", "xts", "spacetime", "FNN", "intervals", "lattice"]
WORKDIR /payload/
COPY ["test_script_gstat/zonal.R", "test_script_gstat/zonal.R"]
CMD ["R", "--vanilla", "-f", "test_script_gstat/zonal.R"]
