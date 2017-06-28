# Copyright 2017 Opening Reproducible Research (http://o2r.info)

#' Stopsignal-instruction class yet to be implemented
#' @include Class-Instruction.R
#'
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#stopsignal}.
#'
#' @return object
#' @export
#' @family instruction classes
#'
#' @examples
#' #no example yet
setClass("Stopsignal", contains = "Instruction")

#' Stopsignal constructor yet to be implemented
#'
#' @param ... fields yet to be implemented
#'
#' @return the object
#' @export
#'
#' @examples
#' #no example yet
Stopsignal <- function(...) {
  stop("Constructor not yet implemented for this class.")
}

setMethod("docker_arguments",
          signature(obj = "Stopsignal"),
          function(obj) {
            stop("The generic function docker_arguments is not implemented for class ",
                 class(obj))
          })
