# Copyright 2016 Opening Reproducible Research (http://o2r.info)

#' An S4 class to represent a Dockerfile's maintainer
#'
#' @slot name The name
#' @slot email The email
#'
#' @return an object of class \code{Maintainer}
#' @export
Maintainer <- setClass("Maintainer",
                       slots = list(name = "character",
                                    email = "character"))

setClassUnion("NullOrMaintainer", c("NULL", "Maintainer"))

#' An S4 class to represent a Dockerfile
#'
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/}.
#'
#' @slot image the base image, used in the FROM statement (character)
#' @slot maintainer the MAINTAINER (object of class maintainer)
#' @slot instructions an ordered list of instructions in the Dockerfile (list of character)
#' @slot path location where the dockerfile is created by default when applying the write-method (character)
#' @slot context Directories that shall be included in the context when dockerfile is build
#'
#' @return an object of class \code{Dockerfile}
#' @export
Dockerfile <- setClass("Dockerfile",
                    slots = list(image = "character",
                                 maintainer = "NullOrMaintainer",
                                 instructions = "list",
                                 path = "character",
                                 context = "character")
)



