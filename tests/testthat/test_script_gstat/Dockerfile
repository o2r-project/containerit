FROM rocker/r-ver:3.3.2
MAINTAINER "matthiashinz"
WORKDIR payload/
COPY ["zonal.R", "zonal.R"]
RUN ["/bin/sh", "-c", "export", "DEBIAN_FRONTEND=noninteractive"]
RUN ["apt-get", "update", "-qq"]
RUN ["apt-get", "install", "-y", "gdal-bin", "libgdal-dev", "libproj-dev"]
RUN ["install2.r", "-r 'https://cran.rstudio.com'", "gstat", "sp", "zoo", "rgdal", "xts", "spacetime", "FNN", "intervals", "lattice"]
CMD ["Rscript", "zonal.R"]
