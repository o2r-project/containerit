# Copyright 2017 Opening Reproducible Research (http://o2r.info)

##set CRAN mirror (the rocker default MRAN is sometimes not up-to-date with current packages)
.default_cran_mirror <- "https://cloud.r-project.org"

#' Get default cran mirror to be used to install R packages
#'
#' @return The mirror's url as single character string (default: "https://cloud.r-project.org")
#' @export
#'
get_docker_cran_mirror <- function() {
  return(.default_cran_mirror)
}


#' Set default cran mirror to be used to install R packages
#'
#' @param cran_url The mirror's url as single character string
#' @export
#'
set_docker_cran_mirror <- function(cran_url){
  .default_cran_mirror <<- cran_url
}

.rocker_images <- c(versioned = "rocker/r-ver",
                   rstudio = "rocker/rstudio",
                   tidyverse = "rocker/tidyverse",
                   verse = "rocker/verse",
                   geospatial = "rocker/geospatial")

##TODO: append more supported images
.supported_images <- .rocker_images

.debian_platform = "linux-x86_64-debian-gcc"

.ubuntu_platform = "linux-x86_64-ubuntu-gcc"

# TODO: Support more platforms analogue to rsysreqs > https://github.com/r-hub/sysreqsdb/tree/master/platforms
## Enhance method .create_run_install with mappings from system requrirements to system comands
.supported_platforms <- .debian_platform



.init_config_file <- function(){
  tryCatch(
    .package_config <- .containeRit_read_config(), #see containeRit-config.R
    error = function(e) {
      message <-
        "ContaineRit config file could not be read. Run containeRit_write_config() to re-initialize the config file."
      message <- paste0(message, "\n\tCaused by: ", e)
      warning(message)
    }
  )
  return(.package_config)
}

.package_config <- .init_config_file() 




