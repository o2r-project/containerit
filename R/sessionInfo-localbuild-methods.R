# Copyright 2018 Opening Reproducible Research (https://o2r.info)

# Shorthand method for creating a local Docker Image based on either an existing Dockerfile (given by folder) or a Dockerfile object
# When a Dockerfile object is written, a temporary file is written in the context directory and deleted after build
# Currently used for testing only.
# Returns the image name.
create_localDockerImage <- function(x,
                                    image_name = strsplit(tempfile(pattern = "containerit_test_", tmpdir = ""), "/")[[1]][2],
                                    use_workdir = FALSE) {
  if (is.character(x)) {
    image_id <- docker_build(context = x,
                 tag = image_name)
    return(image_id)
  } else if (inherits(x, "Dockerfile")) {
    context <- NULL

    if (use_workdir) {
      context <- getwd()
      futile.logger::flog.info("Building Docker image from temporary Dockerfile in working directory %s", context)
      dockerfile_path <- tempfile(pattern = "Dockerfile", tmpdir = context)
    } else {
      tempdir <- tempfile(pattern = "dir")
      futile.logger::flog.info("Building Docker image from temporary Dockerfile and directory")
      context = tempdir
      dir.create(tempdir)
      dockerfile_path <- file.path(tempdir, "Dockerfile")
    }

    write(x, file = dockerfile_path)
    futile.logger::flog.debug("Wrote Dockerfile to %s", dockerfile_path)

    image_id <- docker_build(context = context,
                                tag = image_name)
    futile.logger::flog.debug("Build image: %s", image_id)

    if (use_workdir) {
      futile.logger::flog.info("Deleting temporary Dockerfile...")
      unlink(dockerfile_path, recursive = TRUE)
    } else {
      futile.logger::flog.info("Deleting temporary Dockerfile and directory...")
      unlink(tempdir, recursive = TRUE)
    }

    return(image_id)
  } else {
    stop("Unsupported first argument: ", toString(x))
  }
}

# Obtains a session info from a local R session executed by external system commands with the given expression expr or file
# In any case, a sessioninfo is written to a temporary file and then loaded into the current session
# If a file (R script) is given, the script is copied to a temporary file and the commands to write the sessionInfo are appended
#
# This method is used for packaging R scripts (see dockerFileFromFile)
# and for comparing session information (see test/testthat/test_sessioninfo_repoduce.R)
obtain_localSessionInfo <- function(expr = c(),
           file = NULL, # an R script to be executed
           rmd_file = NULL, # an R Markdown file
           echo = FALSE, # whether R scripts should be 'echoed'
           predetect = TRUE, # whether to use automagic to make sure all required packages are installed
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
