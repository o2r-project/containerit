# Copyright 2018 Opening Reproducible Research (https://o2r.info)

#' An S4 class to represent a Dockerfile's maintainer
#' @include Class-Instruction.R
#' @include Class-Label.R
#'
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#maintainer}.
#'
#' @slot name The name
#' @slot email The email
#'
#' @family instruction classes
#' @family maintainer
#' @return an object of class \code{Maintainer}
#' @export
setClass(
  "Maintainer",
  slots = list(name = "character",
               email = "character"),
  contains = "Instruction"
)

#' create objects of class Maintainer
#'
#' @param name character name (by convention: "<given name> <last name>", e.g. "Matthias Hinz")
#' @param email The email
#'
#' @family maintainer
#' @return Maintainer object
#' @export
#'
Maintainer <- function(name, email = NA_character_) {
  .Deprecated("Label_Maintainer")
  methods::new("Maintainer", name = name, email = email)
}

# The maintainer is optional in a dockerfile
setClassUnion("NullOrLabelOrMaintainer",
              members = c("Maintainer", "NULL", "Label"))

# Specify how class slots are composed to instruction arguments
setMethod(
  "docker_arguments",
  signature = signature(obj = "Maintainer"),
  definition = function(obj) {
    arg <- paste0("\"", obj@name, "\"")
    if (!is.na(obj@email) && length(obj@email) > 0)
      arg <- paste(arg, obj@email)
    return(arg)
  }
)
