# Copyright 2018 Opening Reproducible Research (https://o2r.info)

#' Build a Docker image from a local Dockerfile
#'
#' Uploads a folder with a Dockerfile and supporting files to an instance and builds it.
#' The method is implemented based on \pkg{stevedore}.
#'
#' @param context Local location of build directory including valid Dockerfile
#' @param tag Name of the new image to be created
#' @param dockerfile (optional) set path to the dockerfile (equals to path/to/Dockerfile"))
#' @param ... Other arguments passed to stevedore
#'
#' @return A table of active images on the instance
#' @export
#' @importFrom stevedore docker_client
docker_build <- function(context,
                         tag,
                         the_dockerfile = "Dockerfile",
                         ...) {
  futile.logger::flog.debug("Build dockerfile %s at %s with tag %s", the_dockerfile, context, tag)
  stopifnot(stevedore::docker_available())
  stopifnot(file.exists(file.path(context, the_dockerfile)))

  futile.logger::flog.info("docker build at %s with %s as %s",
                             context, the_dockerfile, tag)

  client <- stevedore::docker_client()
  buildoutput <- client$image$build(context = context, tag = tag, dockerfile = the_dockerfile)
  futile.logger::flog.debug("Build image %s", buildoutput$name())

  buildoutput$id()
}

addInstruction <- function(dockerfileObject, value) {
  instructions <- methods::slot(dockerfileObject, "instructions")
  instructions <- append(instructions, value)
  methods::slot(dockerfileObject, "instructions") <- instructions
  return(dockerfileObject)
}

#' Add one or more instructions to a Dockerfile
#'
#' @param dockerfileObject An object of class 'Dockerfile'
#' @param value An object that inherits from class 'instruction' or a list of instructions
#'
#' @return Returns the modified Dockerfile object (replacement method)
#' @export
#'
#' @examples
#' the_dockerfile <- dockerfile(clean_session())
#' addInstruction(the_dockerfile) <- Label(myKey = "myContent")
"addInstruction<-" <- addInstruction

#' Get R version in string format used for image tags
#'
#' Returns either a version extracted from a given object or the default version.
#'
#' @param from the source to extract an R version: a `sessionInfo()` or `session_info()` object, or an \code{Rdata} file with a session info object
#' @param default if 'from' does not contain version information (e.g. its an Rscript), use this default version information.
#'
#' @export
#'
#' @examples
#' getRVersionTag(from = sessionInfo())
#' getRVersionTag()
getRVersionTag <- function(from, default = paste(R.Version()$major, R.Version()$minor, sep = ".")) {
  r_version <- NULL
  if (inherits(from, "sessionInfo")) {
    r_version <- paste(from$R.version$major, from$R.version$minor, sep = ".")
    futile.logger::flog.debug("Got R version from sessionInfo: %s", r_version)
  } else if (inherits(from, "session_info")) {
    r_version <- stringr::str_extract(pattern = "\\d+(\\.\\d+)+", string = from$platform$version)
    futile.logger::flog.debug("Got R version from session_info: %s", r_version)
  } else if (!is.null(from) && !is.na(from) && file.exists(from) && stringr::str_detect(from, ".Rdata$")) {
    sessionInfo <- getSessionInfoFromRdata(from)
    r_version <- getRVersionTag(sessionInfo)
    futile.logger::flog.debug("Got R version from file %s: %s", from, r_version)
  } else {
    r_version <- default
    futile.logger::flog.debug("Falling back to default R version: %s", r_version)
  }

  return(r_version)
}

#' Reads a \code{sessionInfo} object from an \code{Rdata} file
#'
#' @param file file path
#'
#' @return An object of class \code{sessionInfo}
#' @export
#'
#' @examples
#' sessionInfo <- sessionInfo()
#' file <- tempfile(tmpdir = tempdir(), fileext = ".Rdata")
#' save(sessionInfo, file = file)
#' getSessionInfoFromRdata(file)
#'
getSessionInfoFromRdata <- function(file) {
  futile.logger::flog.info("Reading object 'sessionInfo' from the given file %s", file)
  e1 <- new.env()
  load(file = file, envir = e1)

  futile.logger::flog.debug("Loaded Rdata file with objects %s", toString(ls(envir = e1)))
  if (length(ls(envir = e1)) != 1)
    stop("Provided Rdata file must contain exactly one object.")

  if (!grepl(pattern = "sessionInfo|sessioninfo|session_info", ls(envir = e1)[1]))
    stop("Provided objects must be named sessionInfo|session_info|sessionInfo but have", ls(envir = e1)[1])

  info <- get(ls(envir = e1)[1], envir = e1)

  if (!(inherits(info, "sessionInfo") ||
         inherits(info, "session_info")))
    stop("Provided sessionInfo objects must have class 'sessionInfo' or 'session_info' but is ", class(info))

  return(info)
}

#' Creates an empty R session via system commands and captures the session information
#'
#' @param expr optional list of expressions to be executed in the session
#' @param file optional R script to be executed in the session (uses source-function)
#' @param vanilla indicate wheter the session should be a vanilla R session
#' @param slave whether to run R silentely (here: in slave mode)
#' @param echo wheter to print out detailed information from R
#'
#' @return An object of class session info (Can be used as an input to the dockerfile-method)
#' @export
clean_session <- function(expr = c(),
                          file = NULL,
                          echo = FALSE) {
  obtain_localSessionInfo(expr = expr,
                          file = file,
                          echo = echo)
}
