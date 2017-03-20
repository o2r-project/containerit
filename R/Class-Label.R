# Copyright 2017 Opening Reproducible Research (http://o2r.info)

#' Instruction class representing a LABEL
#' 
#' See Dockerfile reference at https://docs.docker.com/engine/reference/builder/#label
#' See project issue at https://github.com/o2r-project/containerit/issues/42
#' 
#' @include Class-Instruction.R
#'
#' @return object
#' @export
#'
setClass("Label", slots = list(data = "list", multi_line = "logical"), contains = "Instruction")


#' Creates a label of arbitrary content
#' 
#' See https://docs.docker.com/engine/reference/builder/#label
#'
#' @param ... One or more named arguments (key-value pairs), that the Instruction should consist of
#' @param multi_line Whether to print labels with multiple values in one single line or to put each key-value pair in a new line.
#'
#' @return the object
#' @export
#'
#' @examples
#' #no example yet
Label <- function(..., multi_line = FALSE){
  data <- eval(substitute(alist(...)))
  new("Label", data = data, multi_line = multi_line)
}



#' Creates a label holding R session information
#' 
#' See https://docs.docker.com/engine/reference/builder/#label
#'
#' @param session An object of class 'sessionInfo' as returned by utils::sessionInfo(), 
#' or alternatively a class 'session_info' as returned by devtools::session_info()
#' @param as_json Determines whether to parse the session information to a json-string instead of a plain string
#'
#' @return A label with key R_session_info and the deparsed session information in one line
#' @export
#'
#' @examples
#' session <- clean_session()
#' df <- dockerfile(session)
#' addInstruction(df) <- Label_SessionInfo(session)
Label_SessionInfo <- function(session = sessionInfo(), as_json = FALSE){
  if(as_json){
    session_string <- rjson::toJSON(session)
  }else{
    session_string <- utils::capture.output(session)
    session_string <- paste(session_string, collapse = "\n")
  }
  data <- list(R_session_info = session_string)
  new("Label", data = data, multi_line = FALSE)
}



setMethod("docker_arguments",
          signature(obj = "Label"),
          function(obj){
            args <- slot(obj, "data")
            names <- names(args)
          #  values <- sprintf("\"%s\"",as.character(args))
            values <- sapply(as.character(args), deparse)
            output <- paste(names, values, sep = "=")
            if(slot(obj, "multi_line"))
              collapse <- " \\\n\t"
            else
              collapse <- " "
            output <- paste(output, collapse = collapse)
            return(output)
          }
)