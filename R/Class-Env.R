# Copyright 2018 Opening Reproducible Research (https://o2r.info)

setClassUnion("nullOrCharacter", c("NULL", "character"))


#' Env-instruction class yet to be implemented
#' @include Class-Instruction.R
#'
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#env}.
#'
#' @return object
#' @family instruction classes
#' @examples
#' x = Env("myarg", "default value")
#' print(x)
#' x = Env("myarg")
#' print(x)
setClass(
  "Env",
  slots = list(argument = "character",
               value = "nullOrCharacter"),
  contains = "Instruction",
  validity = function(object) {
    if (length(object@argument) == 1 && length(object@value) <= 1) {
      TRUE
      } else {
        "argument must be length 1 and value must be max length 1"
      }
  }
)

#' create objects of class Env
#'
#'
#' @param argument the argument name
#' @param value the value to be set to the argument
#' @export
#' @return Env-object
Env <- function(argument, value = NULL) {
  return(new("Env", argument = argument, value = value))
}


setMethod(
  "docker_key",
  signature = signature(obj = "Env"),
  definition =
    function(obj) {
      return("ENV")
    }
)

setMethod("docker_arguments",
          signature(obj = "Env"),
          function(obj) {
            argument <- methods::slot(obj, "argument")
            value <- methods::slot(obj, "value")
            if (is.null(value)) {
              value = ""
            } else {
              value = paste0('"', value, '"')
            }
            return(paste0(argument, "=", value))
          })

