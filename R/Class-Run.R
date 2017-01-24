# Copyright 2016 Opening Reproducible Research (http://o2r.info)

#' An S4 class to represent a RUN instruction
#' @include Class-Instruction.R
#' 
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#run}.
#'
#' @slot exec character. 
#' @slot params character. 
#'
#' @return object of class Run
#' @export
#'
setClass("Run",
         slots = list(exec = "character",
                      params = "character"), contains = "Instruction")

#' create objects representing a RUN instruction
#'
#' @param exec character argument naming the executable
#' @param params paramterer arguments
#'
#' @return An S4 object of class Run
#' @export
#'
Run <- function(exec, params = NA_character_){
  new("Run",  exec = exec, params = params)
}

setMethod("docker_arguments",
          signature(obj = "Run"),
          .arguments.Cmd_Run #uses the same function as Cmd for now
)

setValidity("Run", 
            method = function(object) {
              exec <- slot(object, "exec")
              params <- slot(object, "params")
              
              if(is.na(exec) || stringr::str_length(exec) == 0)
                return(paste("Exec must be a non-empty string, given was: ", exec))
              else
                if (length(params) == 1 && is.na(params))
                  return(TRUE)
              else
                if ((length(params) > 1 && any(is.na(params))) ||
                   any(stringr::str_length(params) == 0))
                  return("If parameters are given for RUN (optional), they cannot be empty strings or NA")
              else    
                return(TRUE)
            }
)