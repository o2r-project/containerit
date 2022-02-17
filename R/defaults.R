# Copyright 2018 Opening Reproducible Research (https://o2r.info)

.rocker_images <- c(
  versioned = "rocker/r-ver",
  rstudio = "rocker/rstudio",
  tidyverse = "rocker/tidyverse",
  verse = "rocker/verse",
  geospatial = "rocker/geospatial"
)

.debian_images <- c(.rocker_images,
  plumber = "trestletech/plumber" # extends rocker/r-base https://hub.docker.com/r/trestletech/plumber/~/dockerfile
)

.supported_images <- c(.debian_images)

.debian_platform <- "linux-x86_64-debian-gcc"
.ubuntu_platform <- "linux-x86_64-ubuntu-gcc"
.supported_platforms <- c(.debian_platform, .ubuntu_platform)

.init_config_file <- function() {
  tryCatch(
    .package_config <- .containerit_read_config(),
    error = function(e) {
      warning("containerit config file could not be read. Run containerit_write_config() to re-initialize the config file.", "\n\tCaused by: ", e)
    }
  )
  return(.package_config)
}

.package_config <- .init_config_file()
