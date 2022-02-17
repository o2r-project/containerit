# Copyright 2018 Opening Reproducible Research (https://o2r.info)

#' Arg-instruction class yet to be implemented
#' @include Class-Instruction.R
#'
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#arg}.
#'
#' @return object
#' @family instruction classes
#' @examples
#' x = Arg("myarg")
#' print(x)
setClass(
  "Arg",
  slots = list(argument = "character"),
  contains = "Instruction",
  validity = function(object) {
    if (length(object@argument) == 1) TRUE else "argument must be length 1"
  }
  )

#' create objects of class Arg
#'
#'
#' @param argument the argument name
#' @export
#' @return Arg-object
Arg <- function(argument) {
  return(new("Arg", argument = argument))
}

setMethod(
  "docker_key",
  signature = signature(obj = "Arg"),
  definition =
    function(obj) {
      return("ARG")
    }
)


setMethod("docker_arguments",
          signature(obj = "Arg"),
          function(obj) {
            argument <- methods::slot(obj, "argument")
            return(argument)
          })

