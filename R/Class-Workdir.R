# Copyright 2016 Opening Reproducible Research (http://o2r.info)

#' S4 Class representing a WORKDIR instruction
#' @include Class-Instruction.R
#'  
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#workdir}.
#'
#' @return object
#' @export
#'
#' @examples
#' #no example yet
setClass("Workdir", slots = list(path = "character"), contains = "Instruction")


#' Constructor for a WORKDIR instruction
#'
#' @param path The path of the working directory
#'
#' @return the object
#' @export
#'
#' @examples
#' #no example yet
Workdir <- function(path){
  new("Workdir", path = path)
}

setMethod("docker_arguments",
          signature(obj = "Workdir"),
          function(obj) {
              slot(obj, "path")
          }
)
