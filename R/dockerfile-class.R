# Copyright 2016 Opening Reproducible Research (http://o2r.info)

#' Docker Instruction - Class
#'
#' @export
#' 
Instruction <- setClass("Instruction")

setGeneric("docker_key", function(obj) standardGeneric("docker_key"))
setMethod("docker_key", signature = signature(obj="Instruction"), definition = 
           function(obj){
             name=class(obj)
             name=stringr::str_to_upper(name)
             return(name)
           })



setGeneric("docker_arguments", function(obj) standardGeneric("docker_arguments"))


#This is a generic function that constructs the arguments of a docker Instruction as a string
#It must be implemented for sub-classe of 'Instruction' in order to generate string representations
setMethod("docker_arguments",
          signature(obj = "Instruction"),
          function(obj){
            stop("The generic function docker_arguments is not implemented for class ",class(obj))
          }
)


setMethod("toString",
          signature(x="Instruction"), 
          function(x)
          {
            return(paste(docker_key(x), docker_arguments(x)))
          }
)


#' Convert an Instruction-object to a string holding a Docker instruction
#'
#' @param x Instruction
#'
#' @return A single character string
#' @export
#'
setMethod("as.character",
          signature(x="Instruction"), 
          function(x){
            return(paste(docker_key(x), docker_arguments(x)))
          }
)


#' An S4 class to represent a Dockerfile's maintainer
#'
#' @slot name The name
#' @slot email The email
#'
#' @return an object of class \code{Maintainer}
#' @export
Maintainer <- setClass("Maintainer",
                       slots = list(name = "character",
                                    email = "character"), contains = "Instruction")

##The maintainer is optional in a dockerfile
setClassUnion("NullOrMaintainer", members = c("Maintainer", "NULL"))

##Specify how class slots are composed to instruction arguments
setMethod("docker_arguments", signature = signature(obj="Maintainer"),
        definition = function(obj){
        arg=paste0("\"",obj@name,"\"")
          if(length(obj@email>0))
            arg = paste(arg, obj@email)
         return(arg)
        }
)

setClass("Tag", contains = "character")
setClass("Digest", contains = "character")
setClassUnion("Postfix", c("Tag","Digest","NULL"))

#' An S4 class to represent a FROM instrunction relating to a docker image
#'
#' @slot image image-id or image name
#' @slot postfix tag or digest
#'
#' @return an object of class \code{Docker From}
#' @export
From <- setClass("From",
                        slots = list(image = "character",
                                     postfix = "Postfix"), contains = "Instruction")


setMethod("docker_arguments",
          signature(obj = "From"),
          function(obj)
          {
            postfix = slot(obj, "postfix")
            image = slot(obj, "image")
            if (is.null(postfix)) {
              return(image)
            } else if (inherits(postfix, "Tag")) {
              return(paste0(image, ":", postfix))
            }else if (inherits(postfix, "Digest")) {
              return(paste0(image, "@", postfix))
            }
          }
)



docker_arguments.Maintainer = setMethod("docker_arguments", signature = signature(obj="Maintainer"),
                                        definition = function(obj){
                                          arg=paste0("\"",obj@name,"\"")
                                          if(length(obj@email>0))
                                            arg = paste(arg, obj@email)
                                          return(arg)
                                        }
)



#setMethod("toString", signature("Maintainer"), toString.Maintainer)

#' An S4 class to represent a Dockerfile
#'
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/}.
#'
#' @slot image the base image, used in the FROM statement (character)
#' @slot maintainer the MAINTAINER (object of class maintainer)
#' @slot instructions an ordered list of instructions in the Dockerfile (list of character)
#' @slot context Directories that shall be included in the context when dockerfile is build
#'
#' @return an object of class \code{Dockerfile}
#' @export
Dockerfile <- setClass("Dockerfile",
                    slots = list(image = "character",
                                 maintainer = "NullOrMaintainer",
                                 instructions = "list",
                                 context = "character")
)



#setMethod("docker_key", signature(obj = "Maintainer"), function(obj) "FROM")

