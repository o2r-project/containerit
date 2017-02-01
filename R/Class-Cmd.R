# Copyright 2016 Opening Reproducible Research (http://o2r.info)

#' An S4 class to represent a CMD instruction
#' @include Class-Instruction.R
#'
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#cmd}.
#'
#' @slot exec character.
#' @slot params character.
#'
#' @return object of class cmd
#' @export
#'
setClass(
  "Cmd",
  slots = list(exec = "character",
               params = "character"),
  contains = "Instruction"
)

#' create objects representing a CMD instruction
#'
#' @param exec character argument naming the executable
#' @param params paramterer arguments
#'
#' @return An S4 object of class Cmd
#' @export
#'
#' @examples
#' toString(Cmd("R","--vanilla"))
Cmd <- function(exec = NA_character_, params = NA_character_) {
  new("Cmd",  exec = exec, params = params)
}

setValidity(
  "Cmd",
  method = function(object) {
    exec <- slot(object, "exec")
    params <- slot(object, "params")
    
    if (!is.na(exec) &&
        length(exec) == 1 && stringr::str_length(exec) > 0)
      return(TRUE)
    if (is.na(exec) &&
        !any(is.na(params)) &&
        all(stringr::str_length(params) > 0)) {
      return(TRUE)
    }
    if (length(exec) > 1)
      return("More than one exec parameter was given: ", paste(exec, sep = ", "))
    if ((length(params) > 1 && any(is.na(params))) ||
        any(stringr::str_length(params) == 0))
      return("If parameters are given for CMD, they cannot be empty strings or NA")
    
    return(
      "A Cmd instruction must at least have one non-empty exec-argument or one or more parameters given (i.e. as default for an entrypoint)."
    )
  }
)

.arguments.Cmd_Run <- function(obj) {
  # create arcuments in exec form, i.e.
  # ["executable","param1","param2"]
  # or ["param1","param2"] (for CMD as default parameters to ENTRYPOINT)
  
  exec <- slot(obj, "exec")
  params <- slot(obj, "params")
  string <- "["
  if (!is.na(exec)) {
    string <- paste0(string, sprintf('"%s"', exec))
    if (!any(is.na(params)))
      string <- paste0(string, ", ")
  }
  
  if (!any(is.na(params))) {
    paramstr <- sprintf('"%s"', params)
    paramstr <- paste(paramstr, collapse = ", ")
    string <- paste0(string, paramstr)
  }
  string <- paste0(string, "]")
  
  return(string)
}

setMethod("docker_arguments",
          signature(obj = "Cmd"),
          .arguments.Cmd_Run)
