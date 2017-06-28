# Copyright 2017 Opening Reproducible Research (http://o2r.info)

##set CRAN mirror (the rocker default MRAN is sometimes not up-to-date with current packages)
.default_cran_mirror <- "https://cloud.r-project.org"

#' Get default cran mirror to be used to install R packages within containers.
#'
#' @return The mirror's url as single character string (default: "https://cloud.r-project.org")
#' @export
#'
get_container_cran_mirror <- function() {
  return(.default_cran_mirror)
}


#' Set default cran mirror to be used to install R packages within containers.
#'
#' @param cran_url The mirror's url as single character string
#' @export

#'
set_container_cran_mirror <- function(cran_url) {
  .default_cran_mirror <<- cran_url
}

.rocker_images <- c(
  versioned = "rocker/r-ver",
  rstudio = "rocker/rstudio",
  tidyverse = "rocker/tidyverse",
  verse = "rocker/verse",
  geospatial = "rocker/geospatial"
)

.supported_images <- .rocker_images

.debian_platform = "linux-x86_64-debian-gcc"
.ubuntu_platform = "linux-x86_64-ubuntu-gcc"
.supported_platforms <- .debian_platform

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
