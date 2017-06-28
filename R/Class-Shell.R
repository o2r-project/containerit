# Copyright 2017 Opening Reproducible Research (http://o2r.info)

#' Shell-instruction class yet to be implemented
#' @include Class-Instruction.R
#'
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#shell}.
#'
#' @return object
#' @export
#' @family instruction classes
#'
#' @examples
#' #no example yet
setClass("Shell", contains = "Instruction")

#' Shell constructor yet to be implemented
#'
#' @param ... fields yet to be implemented
#'
#' @return the object
#' @export
#'
#' @examples
#' #no example yet
Shell <- function(...) {
  stop("Constructor not yet implemented for this class.")
}

setMethod("docker_arguments",
          signature(obj = "Shell"),
          function(obj) {
            stop("The generic function docker_arguments is not implemented for class ",
                 class(obj))
          })
