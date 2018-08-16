# Copyright 2018 Opening Reproducible Research (https://o2r.info)

#' Detect packages in a Docker image
#'
#' Extracts all installed packages from a Docker image by running it.
#' The method is based on \code{stevedore}.
#'
#' @param image Name of the new image to be run
#'
#' @return A \code{data.frame} with installed packages and their version of the image
#' @export
#' @importFrom stevedore docker_client docker_available
#' @importFrom utils read.csv
get_installed_packages <- function(image) {
  stopifnot(stevedore::docker_available())

  cmd = c("Rscript", "-e", "write.csv(x = as.data.frame(installed.packages())[,c(\"Version\")])")
  futile.logger::flog.info("Running command %s in %s", paste(cmd, collapse = " "), image)

  .client <- stevedore::docker_client()
  .container <- .client$container$run(image = image,
                                      cmd = cmd,
                                      rm = TRUE,
                                      name = "containerit_get_installed_packages")

  pkgs <- read.csv(text = .container$logs)
  names(pkgs) <- c("pkg", "version")
  attributes(pkgs)$image <- image
  return(pkgs)
}
