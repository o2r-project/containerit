# Copyright 2018 Opening Reproducible Research (https://o2r.info)

#' Instruction class representing a LABEL
#'
#' See Dockerfile reference at https://docs.docker.com/engine/reference/builder/#label
#'
#' @include Class-Instruction.R
#'
#' @return object
#' @export
#' @family label
#' @family instruction classes
#'
setClass("Label",
         slots = list(data = "list", multi_line = "logical"),
         contains = "Instruction")

#' Creates a label of arbitrary content
#'
#' See https://docs.docker.com/engine/reference/builder/#label
#'
#' @param ... One or more named arguments (key-value pairs), that the Instruction should consist of
#' @param multi_line Whether to print labels with multiple values in one single line or to put each key-value pair in a new line.
#' @param label_ns Optionally specify a namespace that should be appended to all keys of the label
#'
#' @return the object
#' @family label
#' @export
#'
#' @examples
#' # A simple label that occupies one line:
#' label1 <- Label(key1 = "this", key2 = "that", otherKey = "content")
#' # A multiline label with one key/value pair per line
#' label2 <- Label(
#'  "info.o2r.name" = "myProject_ImageName",
#'  "org.label-schema.name" = "ImageName",
#'  "yet.another_labelname" = "true",
#'  multi_line = TRUE
#' )
#' #label with fixed namespace for all keys
#' label3 <- Label("name"="A name", "description" = "A description", label_ns = "my.label.ns.")
#' the_dockerfile <- dockerfile(clean_session())
#' addInstruction(the_dockerfile) <- list(label1, label2, label3)
#' cat(format(the_dockerfile),sep = "\n")
#'
Label <-
  function(...,
           multi_line = FALSE,
           label_ns = NA_character_) {
    data <- eval(substitute(alist(...)))
    if (!is.na(label_ns))
      names(data) <- paste0(label_ns, names(data))
    methods::new("Label", data = data, multi_line = multi_line)
  }

#' Creates a label holding R session information
#'
#' See https://docs.docker.com/engine/reference/builder/#label
#'
#' @param session An object of class 'sessionInfo' as returned by utils::sessionInfo(),
#' or alternatively a class 'session_info' as returned by devtools::session_info()
#' @param as_json Determines whether to parse the session information to a json-string instead of a plain string
#'
#' @return A label with key 'R.session-info' and the deparsed session information in one line
#' @export
#' @family label
#' @importFrom utils capture.output
#'
#' @examples
#' the_session <- clean_session()
#' the_dockerfile <- dockerfile(the_session)
#' addInstruction(the_dockerfile) <- Label_SessionInfo(the_session)
Label_SessionInfo <-
  function(session = sessionInfo(),
           as_json = FALSE) {
    if (as_json) {
      # session_string <- rjson::toJSON(session)
      session_string <- jsonlite::toJSON(session)
    } else{
      session_string <- utils::capture.output(session)
      session_string <- paste(session_string, collapse = "\n")
    }
    data <- list("R.session-info" = session_string)
    methods::new("Label", data = data, multi_line = FALSE)
  }

setMethod("docker_arguments",
          signature(obj = "Label"),
          function(obj) {
            args <- methods::slot(obj, "data")
            names <- names(args)
            #  values <- sprintf("\"%s\"",as.character(args))
            values <- sapply(as.character(args), deparse)
            output <- paste(names, values, sep = "=")
            if (slot(obj, "multi_line"))
              collapse <- " \\\n\t"
            else
              collapse <- " "
            output <- paste(output, collapse = collapse)
            return(output)
          })

#' Create Label corresponding to the maintainer field
#'
#' See https://docs.docker.com/engine/reference/builder/#maintainer-deprecated
#'
#' @param value Name or email of the maintainer, see specification
#'
#' @return An object of class 'maintainer'
#' @export
#' @family label
#' @family maintainer
#'
#' @examples
#' Label_Maintainer("SvenDowideit@home.org.au")
#'
Label_Maintainer <- function(value) {
  data <- list(maintainer = value)
  methods::new("Label", data = data, multi_line = FALSE)
}
