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
#' @family instruction classes
#' @examples
#' #no example yet
setClass("Copy",
         slots = list(src = "character", dest = "character"),
         contains = "Instruction")

#' Constructor for COPY instruction
#'
#' Copy one or more files or directories to a Docker image.
#'
#' @param src (character vector) list of files or directories to be copied
#' @param dest (character string) destination directory on the Docker image (either absolute or relative to working directory)
#' @param addTrailingSlashes (boolean) add trailing slashes to the given paths if the source is an existing directory
#'
#' @return the object
#' @importFrom fs dir_exists
#' @importFrom stringr str_detect
Copy <- function(src, dest, addTrailingSlashes = TRUE) {
  # directories given as destination must have a trailing slash in Dockerfiles, add it if missing
  sources <- sapply(X = src, FUN = function(source) {
    if (addTrailingSlashes && fs::dir_exists(source) && !stringr::str_detect(source, "/$"))
        return(paste0(source, "/"))
    else return(source)
    })
  names(sources) <- sources

  destination <- dest
  if (addTrailingSlashes && any(fs::dir_exists(src))) {
    if ( !stringr::str_detect(dest, "/$"))
      destination <- paste0(dest, "/")
  }

  methods::new("Copy",  src = sources, dest = destination)
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
