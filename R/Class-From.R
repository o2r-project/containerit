# Copyright 2016 Opening Reproducible Research (http://o2r.info)


setClass("Tag", contains = "character")
setClass("Digest", contains = "character")
setClassUnion("Postfix", c("Tag", "Digest", "NULL"))

#' An S4 class to represent a FROM instrunction relating to a docker image
#'
#' @include Class-Instruction.R
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#from}.
#'
#' @slot image image-id or image name
#' @slot postfix tag or digest
#' 
#' @family instruction classes
#'
#' @return an object of class \code{Docker From}
#' @export
setClass(
  "From",
  slots = list(image = "character",
               postfix = "Postfix"),
  contains = "Instruction"
)


#' create objects of class From
#'
#'
#' @param image image name or image-id
#' @param tag optional image tag (character) - will be ignored if digest is given
#' @param digest optional image digest (character)
#'
#' @return From-object
#' @export
#'
#'
From <- function(image, tag = NULL, digest = NULL) {
  if (!is.null(digest)) {
    return(new(
      "From",
      image = image,
      postfix = new("Digest", digest)
    ))
  } else if (!is.null(tag)) {
    tag <- new("Tag", tag)
    return(new("From", image = image, postfix = tag))
  }
  else
    return(new("From", image = image))
}


#' Parse a From-instruction from an image-argument
#'
#' @param string Single character string that specifies an image in docker-syntax, i.e. <image-id>, <image@digest>, <image:tag>
#'
#' @return From instruction object
#' @export
#'
parseFrom <- function(string) {
  if (stringr::str_detect(string, "@")) {
    split <- stringr::str_split(string, "@")
    split <- unlist(split)
    return(From(image = split[1], digest = split[2]))
  } else if (stringr::str_detect(string, ":")) {
    split <- stringr::str_split(string, ":")
    split <- unlist(split)
    return(From(image = split[1], tag = split[2]))
  } else{
    return(From(string))
  }
}

setMethod("docker_arguments",
          signature(obj = "From"),
          function(obj) {
            postfix <- slot(obj, "postfix")
            image <- slot(obj, "image")
            if (is.null(postfix)) {
              return(image)
            } else if (inherits(postfix, "Tag")) {
              return(paste0(image, ":", postfix))
            } else if (inherits(postfix, "Digest")) {
              return(paste0(image, "@", postfix))
            }
          })