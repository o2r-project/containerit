# Copyright 2017 Opening Reproducible Research (http://o2r.info)

# TODO: If necessary, add one of the following unimplemented classes refering to Docker-instructions: Arg, Onbuild, Stopsignal, Heathcheck, Shell

#' The Docker Instruction - Class
#'
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#format}.
#' @family instruction classes
#'
#' @export
#'
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
  return(paste(docker_key(x), docker_arguments(x)))
}


setMethod("toString",
          signature(x = "Instruction"),
          .toString.Instruction)


#' Convert an Instruction-object to a string holding a Docker instruction
#'
#' @param x Instruction object (of class Run, Cmd, From ...)
#'
#' @return A single character string in Dockerfile syntax
#' @export
#'
setMethod("as.character",
          signature(x = "Instruction"),
          .toString.Instruction)


#' @export
print.Instruction <- function(x, ...) {
  cat(.toString.Instruction(x, ...), sep="\n")
  invisible(x)
}

setMethod("print",
          signature(x = "Instruction"),
          print.Instruction)





