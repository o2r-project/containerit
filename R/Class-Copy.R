# Copyright 2018 Opening Reproducible Research (https://o2r.info)

#' S4 Class representing a COPY-instruction
#'
#' @include Class-Instruction.R
#'
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#copy}.
#
#' @param src (character vector) list of files or directories to be copied
#' @param dest (character string) destination directory on the Docker image (either absolute or relative to working directory)
#'
#' @return object
#' @export
#'
#' @family instruction classes
#'
#' @examples
#' #no example yet
setClass("Copy",
         slots = list(src = "character", dest = "character"),
         contains = "Instruction")

#' Copy one or more files or directories to a Docker image
#'
#' @param src (character vector) list of files or directories to be copied
#' @param dest (character string) destination directory on the Docker image (either absolute or relative to working directory)
#'
#' @return the object
#' @export
#'
#' @examples
#' #no example yet
Copy <- function(src, dest) {
  methods::new("Copy",  src = src, dest = dest)
}

setMethod("docker_arguments",
          signature(obj = "Copy"),
          function(obj) {
            out <- sprintf('"%s"', methods::slot(obj, "src"))
            out <- append(out, sprintf('"%s"', methods::slot(obj, "dest")))
            out <- paste(out, collapse = ", ")
            out <- sprintf("[%s]", out)
          })


setValidity(
  "Copy",
  method = function(object) {
    src <- methods::slot(object, "src")
    dest <- methods::slot(object, "dest")

    if (length(src) < 1) {
      return("Invalid RUN instruction: There must be at least one file / directory given by 'src'")
    }

    if (length(dest) != 1) {
      return(
        "Invalid RUN instruction: There must be exactly one destination folder given by 'dest'"
      )
    }
  }
)
