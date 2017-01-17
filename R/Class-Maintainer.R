# Copyright 2016 Opening Reproducible Research (http://o2r.info)

#' An S4 class to represent a Dockerfile's maintainer
#' @include Class-Instruction.R
#' 
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#maintainer}.
#'
#' @slot name The name
#' @slot email The email
#'
#' @return an object of class \code{Maintainer}
#' @export
setClass("Maintainer",
         slots = list(name = "character",
                      email = "character"),
         contains = "Instruction")


#' create objects of class Maintainer
#'
#' @param name character name (by convention: "<given name> <last name>", e.g. "Matthias Hinz")
#' @param email The email
#'
#' @return Maintainer object
#' @export
#'
Maintainer <- function(name, email = NA_character_){
  new("Maintainer", name = name, email = email)
}

##The maintainer is optional in a dockerfile
setClassUnion("NullOrMaintainer", members = c("Maintainer", "NULL"))

##Specify how class slots are composed to instruction arguments
setMethod("docker_arguments", signature = signature(obj="Maintainer"),
          definition = function(obj){
            arg=paste0("\"",obj@name,"\"")
            if(!is.na(obj@email) && length(obj@email)>0)
              arg = paste(arg, obj@email)
            return(arg)
          }
)