# Copyright 2018 Opening Reproducible Research (https://o2r.info)

#' Build a Docker image from a local Dockerfile or a \code{Dockerfile} object
#'
#' The method is implemented based on \pkg{stevedore} and includes some logging and defaults.
#' If a \code{Dockerfile} object is given, it is saved as a Dockerfile to a temporary directory.
#'
#' @param x the path to a directory with a valid \code{Dockerfile} (i.e. the \code{context}), or a \code{Dockerfile} object
#' @param tag Name of the new image to be created
#' @param ... Other arguments, passed to \code{\link[stevedore]{docker_client}}
#'
#' @return The id of the image
#' @export
#' @importFrom stevedore docker_client
docker_build <- function(x,
                         tag,
                         ...) {
  UseMethod("docker_build", x)
}

#' @param context Local location of build directory including valid Dockerfile
#' @param the_dockerfile Name of the \code{Dockerfile}, defaults to "Dockerfile"
#' @rdname docker_build
#' @export
docker_build.character <- function(x,
                                   tag = strsplit(tempfile(pattern = "containerit_", tmpdir = ""), "/")[[1]][2],
                                   the_dockerfile = "Dockerfile",
                                   ...) {
  futile.logger::flog.info("Build image at %s with %s as %s", x, the_dockerfile, tag)
  stopifnot(stevedore::docker_available())
  stopifnot(file.exists(file.path(x, the_dockerfile)))


  client <- stevedore::docker_client(... = ...)
  buildoutput <- client$image$build(context = x, tag = tag, dockerfile = the_dockerfile)
  futile.logger::flog.debug("Build image %s", buildoutput$name())

  buildoutput$id()
}

#' @param use_workdir if a \code{\link{Dockerfile}} object is given, this can set the context to the current working directory (if \code{TRUE}) or a temporary directory (default)
#' @rdname docker_build
#' @export
docker_build.Dockerfile <- function(x,
                                    tag = strsplit(tempfile(pattern = "containerit_test_", tmpdir = ""), "/")[[1]][2],
                                    use_workdir = FALSE,
                                    clean_up = TRUE) {
  context <- NULL
  if (use_workdir) {
    context <- getwd()
    dockerfile_path <- tempfile(pattern = "Dockerfile", tmpdir = context)
  } else {
    context <- tempfile(pattern = "dir")
    futile.logger::flog.info("Building Docker image from temporary Dockerfile and directory")
    dir.create(context)
    dockerfile_path <- file.path(context, "Dockerfile")
  }
  futile.logger::flog.info("Build image using Dockerfile object at %s as %s", context, tag)

  write(x, file = dockerfile_path)
  futile.logger::flog.debug("Wrote Dockerfile to %s", dockerfile_path)

  image_id <- docker_build(x = context,
                           tag = tag,
                           the_dockerfile = basename(dockerfile_path))
  futile.logger::flog.debug("Build image: %s", image_id)

  if (clean_up) {
    if (use_workdir) {
      futile.logger::flog.info("Deleting temporary Dockerfile...")
      unlink(dockerfile_path, recursive = TRUE)
    } else {
      futile.logger::flog.info("Deleting temporary Dockerfile and directory...")
      unlink(context, recursive = TRUE)
    }
  }

  return(image_id)
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
#' the_dockerfile <- dockerfile(empty_session())
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
#' @importFrom stringr str_detect regex
#'
#' @examples
#' getRVersionTag(from = sessionInfo())
getRVersionTag <- function(from, default = paste(R.Version()$major, R.Version()$minor, sep = ".")) {
  r_version <- NULL
  if (inherits(from, "sessionInfo")) {
    r_version <- paste(from$R.version$major, from$R.version$minor, sep = ".")
    futile.logger::flog.debug("Got R version from sessionInfo: %s", r_version)
  } else if (inherits(from, "session_info")) {
    r_version <- stringr::str_extract(pattern = "\\d+(\\.\\d+)+", string = from$platform$version)
    futile.logger::flog.debug("Got R version from session_info: %s", r_version)
  } else if (!is.null(from)
             && !is.na(from)
             && file.exists(from)
             && stringr::str_detect(string = from,
                                    pattern = stringr::regex(".rdata$", ignore_case = TRUE))) {
    sessionInfo <- extract_session_file(from)
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
#' extract_session_file(file)
#'
extract_session_file <- function(file) {
  futile.logger::flog.info("Reading object 'sessionInfo' from the given file %s", file)
  e1 <- new.env()
  load(file = file, envir = e1)

  futile.logger::flog.debug("Loaded Rdata file with objects %s", toString(ls(envir = e1)))
  if (length(ls(envir = e1)) != 1)
    stop("Provided Rdata file must contain exactly one object.")

  if (!grepl(pattern = "sessionInfo|sessioninfo|session_info", ls(envir = e1)[1]))
    stop("Provided objects must be named sessionInfo|session_info|sessionInfo but have", ls(envir = e1)[1])

  the_info <- get(ls(envir = e1)[1], envir = e1)

  if (!(inherits(the_info, "sessionInfo") ||
         inherits(the_info, "session_info")))
    stop("Provided sessionInfo objects must have class 'sessionInfo' or 'session_info' but is ", class(the_info))

  return(the_info)
}

#' Optains a session info from R executed in a container using the given Docker image and expressions
#'
#' @param docker_image The name of the Docker image to run
#' @param expr optional list of expressions to be executed in the session
#' @param container_dir The directory in the container where a local temp directory is mounted to, for saving the session info to a file (in lack of a COPY instruction)
#' @param file optional R script to be executed in the session (uses source-function)
#' @param container_name The name used to run the container (which will be removed at the end)
#'
#' @note
#' This function was created for test purposes in order to compare sessionInfos.
#'
#' @return An object of class session info
#' @export
#' @examples
#' extract_session_image("rocker/geospatial:3.3.3")
extract_session_image <- function(docker_image,
                                  expr = c(),
                                  container_dir = "/tmp",
                                  local_dir = tempfile(pattern = "dir"),
                                  deleteTempfiles = TRUE,
                                  container_name = "containerit_capturer") {
  result = tryCatch({
    #create local temporary directory
    dir.create(local_dir)
    if (!dir.exists(local_dir))
      stop("Unable to locate temporary directory: ", local_dir)

    #rdata file to which session info shall be written
    container_tempfile =  file.path(container_dir, "capture.Rdata")
    local_docker_tempfile = file.path(local_dir, "capture.Rdata")

    # Write sessioninfo as an object named 'info' to a file
    e1 <- quote(info <- sessionInfo())
    e2 <- quote(save(list = "info", file = tempfile))
    e2[[3]] <- container_tempfile
    expr <- append(expr, (c(e1, e2)))

    #convert to cmd parameters
    expr <- exprToParam(expr)

    cmd <- c("R")
    cmd <- append(cmd, "--vanilla")
    cmd <- append(cmd, expr)

    futile.logger::flog.info("Running R in container to obtain a session info using image %s and command %s",
                             docker_image,
                             paste(cmd, collapse = " "))

    client <- stevedore::docker_client()
    container <- client$container$run(image = docker_image,
                                      cmd = cmd,
                                      host_config = list(binds = c(paste0(local_dir, ":", container_dir))),
                                      name = container_name)

    if (!file.exists(local_docker_tempfile))
      stop("Sessioninfo was not written to file (file missing): ",
           local_docker_tempfile)

    futile.logger::flog.info("Wrote sessioninfo from Docker to %s", local_docker_tempfile)
    load(local_docker_tempfile)
    #clean up
    if (deleteTempfiles)
      unlink(local_dir, recursive = TRUE)

    container$container$remove()

    get("info")
  }, error = function(e) {
    cat("Error obtaining session info:", toString(e), "\n")
    NULL
  }, finally = {
    #
  })

  return(result)
}

#converts an vector or list of R expression into command line parameters for R (batch mode)
exprToParam <- function(expr, e_append = append, to_string = FALSE) {
  #convert from expressions to enquoted strings
  if (to_string)
    #for command line execution, the commands have to be deparsed once more to strings
    expr <-
      sapply(expr, function(x) {
        deparse(x, width.cutoff = 500)
      })
  expr <-
    sapply(expr, function(x) {
      deparse(x, width.cutoff = 500)
    }, simplify = TRUE, USE.NAMES = FALSE)

  if (!is.null(e_append))
    expr <- sapply(expr, function(x) {
      e_append("-e", x)
    })
  return(unlist(as.character(expr)))
}



#' Creates an empty R session
#'
#' @return An object of class \code{sessionInfo}
#'
#' @details Uses \code{\link{clean_session}}
#'
#' @export
empty_session <- function() {
  clean_session(expr = c(), file = NULL, echo = FALSE)
}

#' Obtains a \code{sessionInfo} from a local R session
#'
#' The function may also execute provided expressions or files.
#'
#' @param expr vector of expressions to be executed in the session (see \code{\link{quote}})
#' @param file R script to be executed in the session, uses \code{\link{source}}
#' @param file_rmd R Markdown file to rendered in the session, uses \code{\link[rmarkdown]{render}}
#' @param echo print out detailed information from R
#' @param pretedect whether to use \pkg{automagic} to install missing packaging before executing the R script or R Markdown file
#' @importFrom utils installed.packages install.packages
#' @export
#' @examples
#' clean_session(c(quote(library('lattice'))))
clean_session <- function(expr = c(),
                          file = NULL,
                          rmd_file = NULL,
                          echo = FALSE,
                          predetect = TRUE,
                          repos = "http://cloud.r-project.org",
                          cmd = Sys.getenv("R_HOME")) {
  #append commands to create a local sessionInfo
  required_pkgs <- c()
  if (!is.null(file) && file.exists(file)) {
    expr <- append(expr, call("source", file = file, echo = echo))

    if (predetect) {
      required_pkgs <- automagic::parse_packages(file)
      futile.logger::flog.debug("Analysed input file %s and found required packages: %s",
                                file, toString(required_pkgs))
    }
  }

  if (!is.null(rmd_file) && file.exists(rmd_file)) {
    render_call <- quote(rmarkdown::render("file"))
    render_call[[2]] <- rmd_file #replace the argument "file
    expr <- append(expr, render_call)

    if (predetect) {
      required_pkgs <- automagic::parse_packages(rmd_file)
      futile.logger::flog.debug("Analysed input file %s and found required packages: %s",
                                rmd_file, toString(required_pkgs))
    }
  }

  if (predetect && length(required_pkgs) > 0) {
    installing_pkgs <- stringr::str_remove_all(required_pkgs, "\"")
    installing_pkgs <- setdiff(installing_pkgs, rownames(installed.packages()))
    if (length(installing_pkgs) > 0) {
      futile.logger::flog.info("Missing packages installed before running file using repos %s: %s",
                               toString(repos),
                               toString(installing_pkgs))

      install.packages(pkgs = installing_pkgs, repos = repos)
    } else {
      futile.logger::flog.debug("No missing packages to install before running file")
    }
  }

  futile.logger::flog.info(paste("Creating an R session with the following expressions:\n\t ", toString(expr)))

  info <- callr::r_vanilla(function(expressions) {
    for (e in expressions) {
      eval(e)
    }
    sessionInfo()
  }, args = list(expressions = expr), libpath = .libPaths(), repos = repos)

  if (is.null(info))
    stop("Failed to determine a sessionInfo in a new R session.")

  return(info)
}
