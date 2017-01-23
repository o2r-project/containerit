
##set CRAN mirror (the rocker default MRAN is sometimes not up-to-date with current packages)
.default_cran_mirror <- "https://cran.rstudio.com"

#' Get default cran mirror to be used to install R packages
#'
#' @return The mirror's url as single character string (default: "https://cran.rstudio.com")
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
  .default_cran_mirror <- cran_url
}