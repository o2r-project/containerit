# Copyright 2016 Opening Reproducible Research (http://o2r.info)

#' Instruction class yet to be implemented
#' @include Class-Instruction.R
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#entrypoint}.
#' 
#' @return object
#' @export
#'
#' @examples
#' #no example yet
setClass("Entrypoint", contains = "Instruction")


#' Constructor yet to be implemented
#'
#' @param ... fields yet to be implemented
#'
#' @return the object
#' @export
#'
#' @examples
#' #no example yet
Entrypoint <- function(...){
  stop("Constructor not yet implemented for this class.")
}

setMethod("docker_arguments",
          signature(obj = "Entrypoint"),
          function(obj) {
            stop("The generic function docker_arguments is not implemented for class ",
                 class(obj))
          }
)