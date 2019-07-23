# Copyright 2018 Opening Reproducible Research (https://o2r.info)

#' Onbuild-instruction class yet to be implemented
#' @include Class-Instruction.R
#'
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#onbuild}.
#'
#' @return object
#' @family instruction classes
#' @examples
#' #no example yet
setClass("Onbuild", contains = "Instruction")

#' Onbuild constructor yet to be implemented
#'
#' @param ... fields yet to be implemented
#'
#' @return the object
#' @examples
#' #no example yet
Onbuild <- function(...) {
  stop("Constructor not yet implemented for this class.")
}

setMethod("docker_arguments",
          signature(obj = "Onbuild"),
          function(obj) {
            stop("The generic function docker_arguments is not implemented for class ",
                 class(obj))
          })
