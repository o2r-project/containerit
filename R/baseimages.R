# Copyright 2017 Opening Reproducible Research (http://o2r.info)

#' Detect packages in a Docker image
#'
#' Extracts all installed packages from a Docker image by running it.
#' The method is based on \code{harbor::docker_cmd}.
#'
#' @param host A host object (see harbor-package)
#' @param image Name of the new image to be run
#'
#' @return A \code{data.frame} with installed packages and their version of the image
#' @export
#@importFrom harbor localhost docker_cmd
get_installed_packages <- function(host = harbor::localhost, image) {
  args = c("--rm", image, "Rscript", "-e", "write.csv(as.data.frame(installed.packages())[,c(\"Version\")])")
  futile.logger::flog.info("EXEC: docker run %s", paste(args, collapse = " "))

  output <- harbor::docker_cmd(host = harbor::localhost, cmd = "run", args = args, capture_text = TRUE)
  pkgs <- read.csv(text = output)
  names(pkgs) <- c("pkg", "version")
  attributes(pkgs)$image <- image
  return(pkgs)
}
