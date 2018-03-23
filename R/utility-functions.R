# Copyright 2017 Opening Reproducible Research (http://o2r.info)

#' Build a Docker image from a local Dockerfile
#'
#' Uploads a folder with a Dockerfile and supporting files to an instance and builds it.
#' The method is implemented based on \code{harbor::docker_cmd} and analogue to \code{googleComputeEngineR::docker_build} (with small differences).
#'
#' @param host A host object (see harbor-package)
#' @param dockerfolder Local location of build directory including valid Dockerfile
#' @param tag Name of the new image to be created
#' @param dockerfile (optional) set path to the dockerfile (equals to path/to/Dockerfile"))
#' @param wait Whether to block R console until finished build
#' @param no_cache Wheter to use cached layers to build the image
#' @param docker_opts Additional Docker options
#' @param ... Other arguments passed to the SSH command for the host
#'
#' @return A table of active images on the instance
#' @export
#@importFrom harbor localhost docker_cmd
docker_build <-
  function (host = harbor::localhost,
            dockerfolder,
            tag,
            dockerfile = character(0),
            wait = FALSE,
            no_cache = FALSE,
            docker_opts = character(0),
            ...) {
    # and also handle Dockerfile-Objects as input, analogue to the internal method 'create_localDockerImage'
    stopifnot(dir.exists(dockerfolder))

    docker_opts <- append(docker_opts, c("-t", tag))

    if (length(dockerfile) > 0) {
      stopifnot(file.exists(dockerfile))
      docker_opts <-
        append(docker_opts, c("-f", normalizePath(dockerfile)))
    }

    if (no_cache)
      docker_opts <- append(docker_opts, "--no-cache")

    futile.logger::flog.info("EXEC: docker build %s %s",
                             paste(docker_opts, collapse = " "),
                             dockerfolder)
    .buildoutput <- harbor::docker_cmd(
      host,
      "build",
      args = dockerfolder,
      docker_opts = docker_opts,
      wait = wait,
      capture_text = TRUE,
      ...
    )
    futile.logger::flog.debug("Build output: %s", .buildoutput)

    harbor::docker_cmd(host, "images", ..., capture_text = TRUE)
  }

#' Read low-level information from images and containers
#'
#' See https://docs.docker.com/engine/reference/commandline/inspect/
#'
#' The imlementation is based on \link[harbor]{docker_cmd} in the harbor package
#' @seealso \code{\link[harbor]{docker_cmd}}
#'
#' @param host A host object, as specified by the harbor package
#' @param name Name or id of the docker object (can also be a list of multiple names/ids)
#' @param labelsOnly Whether to restrict the output to the field Config > Labels
#' @param docker_opts Options to docker. These are things that come before the docker command, when run on the command line. (as in harbor::docker:cmd)
#' @param ... Other arguments passed to the SSH command for the host
#'
#' @return A named list of labels for each name or id given. (So, if there are multiple names/ids a list of named lists is returned)
#'
#' @export
#' @examples
#' \dontrun{
#'  docker_inspect(name="rocker/r-ver:3.3.2")
#' }
#'
docker_inspect <- function(host = harbor::localhost,
                           name,
                           labelsOnly = FALSE,
                           docker_opts = character(0),
                           ...) {
  output <-
    harbor::docker_cmd(host,
                       "inspect",
                       args = name,
                       docker_opts,
                       capture_text = TRUE,
                       ...)
  output <- rjson::fromJSON(output)
  if (labelsOnly) {
    output <- sapply(output, function(obj) {
      list(obj[["Config"]][["Labels"]])
    })
  }
  if (is.list(output) && length(output) == 1)
    output <- output[[1]]
  return(output)
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
#' df <- dockerfile(clean_session())
#' addInstruction(df) <- Label(myKey = "myContent")
"addInstruction<-" <- addInstruction

#' Get R version in string format used for image tags
#'
#' Returns either a version extracted from a given object or the default version.
#'
#' @param from the source to extract an R version: a `sessionInfo()` object, or an \code{Rdata} file with a \code{sessionInfo} object
#' @param default if 'from' does not contain version information (e.g. its an Rscript), use this default version information.
#'
#' @export
#'
#' @examples
#' getRVersionTag(from = sessionInfo())
#' getRVersionTag()
getRVersionTag <- function(from = NULL, default = R.Version()) {
  r_version <- NULL
  if (is.null(from)) {
    r_version <- default
  } else if (inherits(from, "sessionInfo")) {
    r_version <- from$R.version
    futile.logger::flog.debug("Got R version from sessionInfo: %s", r_version)
  } else if (file.exists(from) && stringr::str_detect(from, ".Rdata$")) {
    sessionInfo <- getSessionInfoFromRdata(from)
    r_version <- sessionInfo$R.version
    futile.logger::flog.debug("Got R version from file %s: %s", from, r_version)
  } else {
    r_version <- default
    futile.logger::flog.debug("Falling back to default R version: %s", r_version)
  }

  return(paste(r_version$major, r_version$minor, sep = "."))
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
getSessionInfoFromRdata <- function(file) {
  futile.logger::flog.info("Reading object 'sessionInfo' from the given file %s", file)
  e1 <- new.env()
  load(file = file, envir = e1)

  futile.logger::flog.debug("Loaded Rdata file with objects %s", toString(ls(envir = e1)))
  if (!all(grepl(pattern = "sessionInfo", ls(envir = e1))))
    stop("Provided Rdata file must contain only one object of name 'sessionInfo'")

  sessionInfo <- get("sessionInfo", envir = e1)
  if (!inherits(sessionInfo, "sessionInfo"))
    stop("Provided sessionInfo objects must have class 'sessionInfo' but is ", class(sessionInfo))

  return(sessionInfo)
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
clean_session <- function(expr = list(),
                          file = NULL,
                          vanilla = TRUE,
                          slave = FALSE,
                          echo = FALSE) {
  obtain_localSessionInfo(expr = expr,
                          file = file,
                          slave = slave,
                          echo = echo,
                          vanilla = vanilla)
}
