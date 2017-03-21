# Copyright 2016 Opening Reproducible Research (http://o2r.info)


#' An S4 class to represent a Dockerfile
#' @include Class-Maintainer.R
#' @include Class-Cmd.R
#' @include Class-From.R
#' @include Class-Instruction.R
#'
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/}.
#'
#' @slot image the base image, used in the FROM statement (character)
#' @slot maintainer a maintainer label or an object of class maintainer (the latter is debrecated)
#' @slot instructions an ordered list of instructions in the Dockerfile (list of character)
#' @slot context Directories that shall be included in the context when Dockerfile is build
#' @slot cmd the default cmd instruction applied to the container
#'
#' @return an object of class \code{Dockerfile}
#' @export
Dockerfile <- setClass("Dockerfile",
                    slots = list(image = "From",
                                 maintainer = "NullOrLabelOrMaintainer",
                                 instructions = "list",
                                 context = "character",
                                 cmd = "Cmd")
)
