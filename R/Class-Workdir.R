# Copyright 2018 Opening Reproducible Research (https://o2r.info)

#' S4 Class representing a WORKDIR instruction
#' @include Class-Instruction.R
#'
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#workdir}.
#'
#' @return object
#' @family instruction classes
setClass("Workdir",
         slots = list(path = "character"),
         contains = "Instruction")

#' Constructor for a WORKDIR instruction
#'
#' @param path The path of the working directory
#'
#' @return the object
#'
#' @examples
#' instruction <- containerit:::Workdir("~/myDir/subdir/")
#' toString(instruction)
Workdir <- function(path) {
  methods::new("Workdir",
               # directories given as destination must have a trailing slash in Dockerfiles
               path = ifelse(stringr::str_detect(path, "/$"), yes = path, no = paste0(path, "/")))
}

setMethod("docker_arguments",
          signature(obj = "Workdir"),
          function(obj) {
            methods::slot(obj, "path")
          })
