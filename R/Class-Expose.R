# Copyright 2016 Opening Reproducible Research (http://o2r.info)

#' Instruction class yet to be implemented
#' @include Class-Instruction.R
#' 
#' @return object
#' @export
#'
setClass("Expose", contains = "Instruction")


#' Constructor yet to be implemented
#'
#' @param ... fields yet to be implemented
#'
#' @return the object
#' @export
#'
#' @examples
#' #no example yet
Expose <- function(...){
  stop("Constructor not yet implemented for this class.")
}

setMethod("docker_arguments",
          signature(obj = "Expose"),
          function(obj){
            stop("The generic function docker_arguments is not implemented for class ",class(obj))
          }
)