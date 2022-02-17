# Copyright 2018 Opening Reproducible Research (https://o2r.info)

# put this here because used in many others
setClassUnion("NullOrCharacter", c("NULL", "character"))

#' The Docker Instruction - Class
#'
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#format}.
#' @family instruction classes
Instruction <- setClass("Instruction", contains = "VIRTUAL")

setGeneric("docker_key", function(obj)
  standardGeneric("docker_key"))
setMethod(
  "docker_key",
  signature = signature(obj = "Instruction"),
  definition =
    function(obj) {
      name <- class(obj)
      name <- stringr::str_to_upper(name)
      return(name)
    }
)

setGeneric("docker_arguments",
           function(obj)
             standardGeneric("docker_arguments"))

#This is a generic function that constructs the arguments of a docker Instruction as a string
#It must be implemented for sub-classe of 'Instruction' in order to generate string representations
setMethod("docker_arguments",
          signature(obj = "Instruction"),
          function(obj) {
            stop("The generic function docker_arguments is not implemented for class ",
                 class(obj))
          })

#Convert an Instruction-object to a string holding a Docker instruction
.toString.Instruction <- function(x, ...) {
  instruction <- paste0(docker_key(x), " ", docker_arguments(x))
  return(stringr::str_trim(instruction))
}

#' Convert an Instruction-object to a string holding a Docker instruction
#'
#' @param x Instruction object (of class Run, Cmd, From ...)
#' @param ... Arguments to be passed down to toString
#'
#' @return A single character string in Dockerfile syntax
setMethod("toString",
          signature(x = "Instruction"),
          .toString.Instruction)

#' Convert an Instruction-object to a string holding a Docker instruction
#'
#' @param x Instruction object (of class Run, Cmd, From ...)
#' @param ... Arguments to be passed down to toString
#'
#' @return A single character string in Dockerfile syntax
setMethod("as.character",
          signature(x = "Instruction"),
          .toString.Instruction)

print.Instruction <- function(x, ...) {
  cat(.toString.Instruction(x, ...), sep = "\n")
  invisible(x)
}

#' Print an Instruction
#'
#' @param x Instruction.
#' @param ... Arguments to be passed down to toString
setMethod("print",
          signature(x = "Instruction"),
          print.Instruction)
