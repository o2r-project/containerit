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
#' @param clean_up if \code{use_workdir}, then delete the Dockerfile file in the working directory, otherwise remove the Dockerfile created as a temporary file
#' @rdname docker_build
#' @export
docker_build.Dockerfile <- function(x,
                                    tag = strsplit(tempfile(pattern = "containerit_test_", tmpdir = ""), "/")[[1]][2],
                                    use_workdir = FALSE,
                                    clean_up = TRUE,
                                    ...) {
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
#' the_dockerfile <- dockerfile(clean_session())
#' addInstruction(the_dockerfile) <- Label(myKey = "myContent")
"addInstruction<-" <- addInstruction

#' getImageForVersion-method
#'
#' Get a suitable Rocker image based on the R version.
#' Needs network access to retrieve the available images.
#'
#' If there was no matching image found, a warning is issued.
#'
#' @param r_version A string representation of the R version, e.g. "3.4.2"
#' @param nearest A boolean, should the closest version be returned if there is no match?
#'
#' @return A string with the name of the Docker image
#' @export
#' @examples
#' getImageForVersion(getRVersionTag(utils::sessionInfo()))
#' getImageForVersion("3.4.3")
#'
#' @importFrom semver parse_version
getImageForVersion <- function(r_version, nearest = TRUE) {
  #check if dockerized R version is available (maybe check other repositories too?)
  tags <- .tagsfromRemoteImage(.rocker_images[["versioned"]])
  image <- From(.rocker_images[["versioned"]], tag = r_version)

  closestMatch <- function(version, versions) {
    if (version %in% versions) return(version);

    factors <- list(major = 1000000, minor = 1000, patch = 1)

    semver <- semver::parse_version(version)[[1]]
    semver_num <- semver$major * factors[["major"]] +
      semver$minor * factors[["minor"]] +
      semver$patch * factors[["patch"]]

    sorted_semvers <- sort(semver::parse_version(versions))

    offsets <- sapply(X = sorted_semvers, FUN = function(v) {
      v_num <- v$major * factors[["major"]] +
        v$minor * factors[["minor"]] +
        v$patch * factors[["patch"]]
      return(abs(semver_num - v_num))
    })

    min_offset = min(offsets)
    return(sorted_semvers[which(offsets == min_offset)])
  }

  if (!r_version %in% tags) {
    if (nearest) {
      # get numeric versions with all parts (maj.min.minor), i.e. two dots
      numeric_tags <- tags[which(grepl("\\d.\\d.\\d", tags))]
      closest <- as.character(closestMatch(r_version, numeric_tags))
      image <- From(.rocker_images[["versioned"]], tag = closest)

      warning("No Docker image found for the given R version, returning closest match: ",
              closest,
              " Existing tags (list only available when online): ",
              paste(tags, collapse = " ")
      )
    } else {
      warning("No Docker image found for the given R version, returning input. ",
              "Existing tags (list only available when online): ",
              paste(tags, collapse = " ")
      )
    }
  }

  return(image)
}

#' Get R version in string format used for image tags
#'
#' Returns either a version extracted from a given object or the default version.
#'
#' @param from the source to extract an R version: a `sessionInfo()` or `session_info()` object, a `description` object, or an `RData` file with a session info object
#' @param default if 'from' does not contain version information (e.g. its an Rscript), use this default version information.
#'
#' @export
#' @importFrom stringr str_detect regex
#' @importFrom desc desc
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
  } else if (inherits(from, "description")) {
    # get R: stringr::str_extract(string = "methods, R (1.2.3,test), test (9.9)", pattern = "R( \\(.*?\\))?")
    # get version: stringr::str_extract(string = "R (1.2.3)", pattern = "(?<=\\().+?(?=\\))")
    r_depends <- stringr::str_extract(string = from$get_field("Depends"), pattern = "R( \\(.*?\\))?")
    r_version <- stringr::str_extract(string = stringr::str_extract(string = r_depends,
                                                                    pattern = "(?<=\\().+?(?=\\))"),
                                      pattern = "(\\d).*") # everything after the first digit
  } else if (!is.null(from)
             && !is.expression(from)
             && !is.na(from)
             && file.exists(from)) {
    if (basename(from) == "DESCRIPTION") {
      description <- desc::desc(file = from)
      return(getRVersionTag(from = description))
    } else if (stringr::str_detect(string = from,
                                    pattern = stringr::regex(".rdata$", ignore_case = TRUE))) {
      sessionInfo <- extract_session_file(from)
      r_version <- getRVersionTag(sessionInfo)
    }
    futile.logger::flog.debug("Got R version from file %s: %s", from, r_version)
  }

  if (is.null(r_version)) {
    r_version <- default
    futile.logger::flog.debug("Falling back to default R version: %s", r_version)
  }

  return(r_version)
}

#' Reads a \code{sessionInfo} object from an \code{RData} file
#'
#' @param file file path
#'
#' @return An object of class \code{sessionInfo}
#' @export
#'
#' @examples
#' sessionInfo <- sessionInfo()
#' file <- tempfile(tmpdir = tempdir(), fileext = ".RData")
#' save(sessionInfo, file = file)
#' extract_session_file(file)
#'
extract_session_file <- function(file) {
  futile.logger::flog.info("Reading object 'sessionInfo' from the given file %s", file)
  e1 <- new.env()
  load(file = file, envir = e1)

  futile.logger::flog.debug("Loaded RData file with objects %s", toString(ls(envir = e1)))
  if (length(ls(envir = e1)) != 1)
    stop("Provided RData file must contain exactly one object.")

  if (!grepl(pattern = "sessionInfo|sessioninfo|session_info|info|session", ls(envir = e1)[1]))
    stop("Provided objects must be named sessionInfo|session_info|sessionInfo|info|session but have", ls(envir = e1)[1])

  the_info <- get(ls(envir = e1)[1], envir = e1)

  if (!(inherits(the_info, "sessionInfo") || inherits(the_info, "session_info")))
    stop("Provided sessionInfo objects must have class 'sessionInfo' or 'session_info' but is ", class(the_info))

  return(the_info)
}

#' Optains a session info from R executed in a container
#'
#' The function uses the given Docker image and executes the given expressions.
#' To access the session information, a directory is mounted from the host into the container, to which the R session in the container saves an RData file with the sessionInfo object.
#'
#' @param docker_image The name of the Docker image to run
#' @param expr A list of expressions to be executed in the session
#' @param container_dir The directory in the container where a local temp directory is mounted to, for saving the session info to a file (in lack of a COPY instruction)
#' @param local_dir The local directory mounted into the container
#' @param deleteTempfiles Remove used \code{local_dir} directory when done
#' @param container_name The name used to run the container (which will be removed at the end)
#'
#' @note
#' This function was created for test purposes in order to compare sessionInfos.
#'
#' @return An object of class session info
#' @export
#' @examples
#' extract_session_image("rocker/geospatial:3.3.3")
#' @importFrom fs dir_exists
extract_session_image <- function(docker_image,
                                  expr = c(),
                                  container_dir = "/tmp",
                                  local_dir = tempfile(pattern = "extract_bind_"),
                                  deleteTempfiles = TRUE,
                                  container_name = "containerit_capturer") {
  result = tryCatch({
    #create local temporary directory
    dir.create(local_dir)
    if (!fs::dir_exists(local_dir))
      stop("Unable to locate temporary directory: ", local_dir)

    #rdata file to which session info shall be written
    container_tempfile =  file.path(container_dir, "capture.RData")
    local_docker_tempfile = file.path(local_dir, "capture.RData")

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
      stop("Sessioninfo was not written to file (file missing): ", local_docker_tempfile)

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

#' Obtains a \code{sessionInfo} from a local R session
#'
#' The function may also execute provided expressions or files.
#' The implementation is based on \code{\link[callr]{r_vanilla}}.
#'
#' @param expr vector of expressions to be executed in the session (see \code{\link{quote}})
#' @param script_file R script to be executed in the session, uses \code{\link{source}}
#' @param rmd_file R Markdown file to rendered in the session, uses \code{\link[rmarkdown]{render}}
#' @param echo print out detailed information from R
#' @param predetect whether to use \pkg{automagic} to install missing packaging before executing the R script or R Markdown file
#' @param repos Repository to use, required if \code{expr} includes install statements
#' @importFrom utils installed.packages install.packages
#' @export
#' @examples
#' clean_session(c(quote(library('lattice'))))
clean_session <- function(expr = c(),
                          script_file = NULL,
                          rmd_file = NULL,
                          echo = FALSE,
                          predetect = TRUE,
                          repos = "https://cloud.r-project.org") {
  #append commands to create a local sessionInfo
  required_pkgs <- c()
  if (!is.null(script_file) && file.exists(script_file)) {
    expr <- append(expr, call("source", file = script_file, echo = echo))

    if (predetect) {
      required_pkgs <- automagic::parse_packages(script_file)
      futile.logger::flog.debug("Analysed input file %s and found required packages: %s",
                                script_file, toString(required_pkgs))
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

  temp_lib_path <- NULL
  if (predetect && length(required_pkgs) > 0) {
    installing_pkgs <- stringr::str_remove_all(required_pkgs, "\"")
    installing_pkgs <- setdiff(installing_pkgs, rownames(installed.packages()))

    temp_lib_path <- tempfile("test_lib_")
    dir.create(temp_lib_path)

    if (length(installing_pkgs) > 0) {
      futile.logger::flog.info("Missing packages will be installed to %s before running file using repos %s: %s",
                               temp_lib_path,
                               toString(repos),
                               toString(installing_pkgs))

      install_log <- capture.output({
        install.packages(pkgs = installing_pkgs, lib = c(temp_lib_path), repos = repos,
                         quiet = TRUE)
      })
      futile.logger::flog.debug("Installation log:\n%s", toString(install_log))
    } else {
      futile.logger::flog.debug("No missing packages to install before running file")
    }
  }

  futile.logger::flog.debug("Creating an R session with the following expressions:\n%s", toString(expr))

  the_info <- callr::r_vanilla(function(expressions) {
    for (e in expressions) {
      eval(e)
    }
    sessionInfo()
  }, args = list(expressions = expr), libpath = c(temp_lib_path, .libPaths()), repos = repos)

  if (is.null(the_info))
    stop("Failed to determine a sessionInfo in a new R session.")
  else futile.logger::flog.debug("Captured sessionInfo:\n%s", capture.output(print(the_info)))

  if ( !is.null(temp_lib_path)) {
    futile.logger::flog.debug("Deleting temp library at %s", temp_lib_path)
    unlink(temp_lib_path)
  }

  return(the_info)
}
