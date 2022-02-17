# Copyright 2018 Opening Reproducible Research (https://o2r.info)

# The Entrypoint is optional in a Dockerfile
setClassUnion("NullOrCmd",
              members = c("Cmd", "NULL"))

#' An S4 class to represent a Dockerfile
#'
#' @include Class-Maintainer.R
#' @include Class-Cmd.R
#' @include Class-From.R
#' @include Class-Instruction.R
#' @include Class-Entrypoint.R
#'
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/}.
#'
#' @slot image the base image, used in the FROM statement (character)
#' @slot maintainer a maintainer label or an object of class maintainer (the latter is debrecated)
#' @slot instructions an ordered list of instructions in the Dockerfile (list of character)
#' @slot entrypoint the entrypoint instruction applied to the container
#' @slot cmd the default cmd instruction applied to the container
#' @slot syntax the syntax given for the header of the container
#'
#' @details The entrypoint and cmd are provided outside of instructions, as only one of them takes effect.
#' If Cmd or Entrypoint instructions are provided as part of the regular instructions, they appear in the Dockerfile but have no effect.
#'
#' @return an object of class \code{Dockerfile}
#' @export
Dockerfile <- setClass("Dockerfile",
                       slots = list(
                         image = "From",
                         maintainer = "NullOrLabelOrMaintainer",
                         instructions = "list",
                         entrypoint = "NullOrEntrypoint",
                         cmd = "NullOrCmd",
                         syntax = "NullOrCharacter")
)

toString.Dockerfile <- function(x, ...) {
  #initialize dockerfile with from
  output <- c()
  syntax <- methods::slot(x, "syntax")
  if (!is.null(syntax)) {
    syntax = trimws(syntax)
    syntax = sub("^#*", "", syntax)
    syntax = trimws(syntax)
    syntax = sub("syntax=", "", syntax)
    syntax = trimws(syntax)
    syntax = paste0("# syntax=", syntax)
    output <- append(output, syntax)
  }
  from <- toString(methods::slot(x, "image"))
  output <- append(output, from)
  maintainer <- methods::slot(x, "maintainer")
  if (!is.null(maintainer))
    output <- append(output, toString(maintainer))
  instructions <- slot(x, "instructions")
  if (!is.null(instructions) && length(instructions) > 0) {
    instructions <- sapply(instructions, toString)
    output <- append(output, unlist(instructions))
  }
  entrypoint <- methods::slot(x, "entrypoint")
  if (!is.null(entrypoint))
    output <- append(output, toString(entrypoint))
  cmd <- methods::slot(x, "cmd")
  if (!is.null(cmd))
    output <- append(output, toString(cmd))
  return(output)
}

print.Dockerfile <- function(x, ...) {
  cat(toString.Dockerfile(x, ...), sep = "\n")
  invisible(x)
}

format.Dockerfile <- function(x, ...) {
  format(toString(x), ...)
}

.write.Dockerfile <-
  function(x, file = file.path(getwd(), "Dockerfile")) {
    return(write(toString(x), file))
  }


#' Format a Dockerfile
#'
#' @param x Dockerfile
#' @param ... Arguments to be passed down to format.default
#'
#' @export
setMethod("format", signature(x = "Dockerfile"), format.Dockerfile)


#' Convert a Dockerfile to String
#'
#' @param x Dockerfile.
#' @param ... Other params (not in use currently)
#'
#' @export
setMethod("toString",
          signature(x = "Dockerfile"),
          toString.Dockerfile)


#' Convert a Dockerfile to String
#'
#' @param x Dockerfile.
#' @param ... Arguments to be passed down to toString
#'
#' @export
setMethod("as.character",
          signature(x = "Dockerfile"),
          toString.Dockerfile)


#' Print a Dockerfile
#'
#' @param x The Dockerfile to be printed
#' @param ... Parameters to be passed down to toString
#'
#' @export
setMethod("print",
          signature(x = "Dockerfile"),
          print.Dockerfile)


#' Write a Dockerfile object to a dockerfile
#'
#' @param x Dockerfile object to be serialized
#' @param file optional argument specifying a costum file path
#'
#' @examples
#' # write a dockerfile with default parameters to temporary file and show content:
#' temp = tempfile()
#' write(dockerfile(), file=temp)
#' print(readLines(temp))
#'
#' @export
setMethod("write", signature(x = "Dockerfile"), .write.Dockerfile)
