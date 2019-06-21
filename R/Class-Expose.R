# Copyright 2018 Opening Reproducible Research (https://o2r.info)

#' S4 Class representing an EXPOSE-instruction
#'
#' @include Class-Instruction.R
#' @include Class-All.R
#'
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#copy}.
#
#' @param port (character or numeric, required) the port of the container to be exposed
#' @param host (numeric, optional) port of the host, if missing the same port as in the container is exposed
#'
#' @family instruction classes
#' @return object
#' @export
#'
#' @examples
#' #no example yet
setClass("Expose",
         slots = list(port = "characterOrInteger", host = "integer"),
         contains = "Instruction")


#' Constructor for Expose
#'
#' @param port (character or numeric, required) the port of the container to be exposed
#' @param host (numeric, optional) port of the host, if missing the same port as in the container is exposed
#'
#' @return the object
#' @export
#'
#' @examples
#' #no example yet
Expose <- function(port, host = NA_integer_) {
  if(is.numeric(port))
    methods::new("Expose", port = as.integer(port), host = as.integer(host))
  else
    methods::new("Expose", port = as.character(port), host = as.integer(host))
}

setMethod("docker_arguments",
          signature(obj = "Expose"),
          function(obj) {
            stop("The generic function docker_arguments is not implemented for class ",
                 class(obj))
          })

setMethod("docker_arguments",
          signature(obj = "Expose"),
          function(obj) {
            out <- sprintf("%s", methods::slot(obj, "port"))
            if(!is.na(obj@host)) {
              out <- append(sprintf('%s', methods::slot(obj, "host")), out)
            }
            out <- paste(out, collapse = " ")
            out <- sprintf("%s", out)
            out
          })

setValidity(
  "Expose",
  method = function(object) {
    port <- methods::slot(object, "port")
    host <- methods::slot(object, "host")

    if (length(port) != 1) {
      return("Invalid EXPOSE instruction: exactly one 'port' is required.")
    }
    if (length(host) != 1) {
      return("Invalid EXPOSE instruction: exactly one 'host' is required.")
    }
    if (is.character(port) && stringr::str_detect(string = port, pattern = "/")) {
      protocol <- stringr::str_split(string = port, pattern = "/")[[1]][2]
      protocol <- stringr::str_to_lower(protocol)
      str
      if(!(protocol == "udp" || protocol == "tcp")) {
        return("Invalid EXPOSE instruction: protocol of container must be 'tcp' or 'udp'")
      }
    }

    return(TRUE)
  }
)
