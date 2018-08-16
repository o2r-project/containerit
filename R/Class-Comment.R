# Copyright 2018 Opening Reproducible Research (https://o2r.info)

#' Instructionclass representing a comment
#'
#' See Dockerfile reference at https://docs.docker.com/engine/reference/builder/#format
#'
#' @include Class-Instruction.R
#'
#' @return object
#' @export
#' @family instruction classes
#'
setClass("Comment",
         slots = list(text = "character"),
         contains = "Instruction")

#' Creates a comment with character content
#'
#' @param text The comment text
#'
#' @return the object
#' @family comment
#' @export
#'
#' @examples
#' # A simple label that occupies one line:
#' comment <- Comment(text = "this is important to note")
#' the_dockerfile <- dockerfile(empty_session())
#' addInstruction(the_dockerfile) <- comment
#' print(the_dockerfile)
#'
Comment <- function(text) {
  methods::new("Comment", text = text)
}

setMethod("docker_arguments",
          signature(obj = "Comment"),
          function(obj) {
            text <- methods::slot(obj, "text")
            return(text)
          })

setMethod("docker_key",
          signature = signature(obj = "Comment"),
          definition = function(obj) {
            return("#")
          }
)
