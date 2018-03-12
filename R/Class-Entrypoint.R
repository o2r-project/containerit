# Copyright 2017 Opening Reproducible Research (http://o2r.info)

#' Entrypoint class yet to be implemented
#' @include Class-Instruction.R
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#entrypoint}.
#'
#' @slot program character, name or path of the executable or command.
#' @slot params list of parameters.
#' @export
#'
#' @family instruction classes
#'
#' @examples
#' #no example yet
setClass("Entrypoint",
         slots = list(program = "character",
                      params = "list",
                      form = "character"),
         contains = "Instruction")

# The Entrypoint is optional in a Dockerfile
setClassUnion("NullOrEntrypoint",
              members = c("Entrypoint", "NULL"))

#' Constructor for Entrypoint yet to be implemented
#'
#' @param program the executable or command
#' @param params the parameters provided to the executable or command
#' @param form the form how the instruction is rendered, either \code{exec} or \code{shell}
#'
#' @return an object of class Entrypoint
#' @export
#'
#' @examples
#' toString(Entrypoint("Rscript", params = list("sum", "1", "2")))
#' toString(Entrypoint("/bin/echo"))
Entrypoint <- function(program, params = list(), form = "exec") {
  if ( !(form == "exec" || form == "shell"))
    stop("Unsupported form '", form, "'. Only 'exec' or 'shell' are supported.")
  methods::new("Entrypoint",  program = program, params = params, form = form)
}

# create arcuments in exec form, i.e. ["executable","param1","param2"]
.arguments_entrypoint_exec <- function(obj) {
  program <- methods::slot(obj, "program")
  params <- methods::slot(obj, "params")

  string <- "["
  string <- paste0(string, sprintf('"%s"', program)) # program is required

  if (!any(is.na(params)) && length(params) > 0) {
    string <- paste0(string, ", ")
    paramstr <- sprintf('"%s"', params)
    paramstr <- paste(paramstr, collapse = ", ")
    string <- paste0(string, paramstr)
  }
  string <- paste0(string, "]")

  return(string)
}

# create arcuments in shell form, i.e. "executable param1 param2"
.arguments_entrypoint_shell <- function(obj) {
  program <- methods::slot(obj, "program")
  params <- methods::slot(obj, "params")

  string <- program # program is required

  if (!any(is.na(params)) && length(params) > 0) {
    paramstr <- sprintf('%s', params)
    paramstr <- paste(paramstr, collapse = " ")
    string <- paste0(string, " ", paramstr)
  }

  return(string)
}

setMethod("docker_arguments",
          signature(obj = "Entrypoint"),
          function(obj) {
            form <- methods::slot(obj, "form")
            if(form == "exec") {
              return(.arguments_entrypoint_exec(obj))
            } else if (form == "shell") {
              return(.arguments_entrypoint_shell(obj))
            } else {
              stop("The provided 'form' ", form, " is not supported.")
            }
          })
