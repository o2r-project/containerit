FROM rocker/r-ver:3.4.4
LABEL maintainer="o2r"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y gdal-bin \
	git-core \
	libgdal-dev \
	libproj-dev \
	pandoc \
	pandoc-citeproc
RUN ["install2.r", "abind", "ade4", "adehabitatLT", "adehabitatMA", "backports", "bdsmatrix", "bitops", "boot", "caTools", "CircStats", "cshapes", "deldir", "digest", "diveMove", "DoseFinding", "evaluate", "FNN", "foreign", "Formula", "geosphere", "goftest", "gstat", "htmltools", "intervals", "KernSmooth", "knitr", "lattice", "lmtest", "magrittr", "mapdata", "maps", "maptools", "MASS", "Matrix", "MatrixModels", "maxLik", "mgcv", "miscTools", "mvtnorm", "nlme", "plm", "plyr", "polyclip", "quadprog", "quantreg", "raster", "RColorBrewer", "Rcpp", "rgdal", "rmarkdown", "rpart", "rprojroot", "sandwich", "SEL", "sp", "spacetime", "SparseM", "spatstat", "spatstat.data", "spatstat.utils", "stringi", "stringr", "tensor", "trip", "uniReg", "xts", "yaml", "zoo"]
WORKDIR /payload/
CMD ["R"]
