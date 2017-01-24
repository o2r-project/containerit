# Copyright 2016 Opening Reproducible Research (http://o2r.info)

#' Instruction class yet to be implemented
#' @include Class-Instruction.R
#'  
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#workdir}.
#'
#' @return object
#' @export
#'
#' @examples
#' #no example yet
setClass("Workdir", contains = "Instruction")


#' Constructor yet to be implemented
#'
#' @param ... fields yet to be implemented
#'
#' @return the object
#' @export
#'
#' @examples
#' #no example yet
Workdir <- function(...){
  stop("Constructor not yet implemented for this class.")
}

setMethod("docker_arguments",
          signature(obj = "Workdir"),
          function(obj) {
            stop("The generic function docker_arguments is not implemented for class ",
                 class(obj))
          }
)
