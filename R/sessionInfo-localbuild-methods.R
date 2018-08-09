# Copyright 2018 Opening Reproducible Research (https://o2r.info)

# Shorthand method for creating a local Docker Image based on either an existing Dockerfile (given by folder) or a Dockerfile object
# When a Dockerfile object is written, a temporary file is written in the context directory and deleted after build
# Currently used for testing only.
create_localDockerImage <- function(x,
                                    host = harbor::localhost,
                                    image_name = strsplit(tempfile(pattern = "containerit_test", tmpdir = ""), "/")[[1]][2],
                                    no_cache = FALSE,
                                    use_workdir = FALSE) {
  if (is.character(x))
    docker_build(
      harbor::localhost,
      dockerfolder = x,
      tag = image_name,
      wait = TRUE
    )
  if (inherits(x, "Dockerfile")) {
    tempdir <- tempfile(pattern = "dir")

    if (use_workdir) {
      context = getwd()
      futile.logger::flog.info("Building Docker image from temporary Dockerfile in context directory:\n\t%s", context)
      dockerfile_path = tempfile(pattern = "Dockerfile", tmpdir = context)
    } else {
      futile.logger::flog.info("Building Docker image from temporary Dockerfile and directory")
      context = tempdir
      dir.create(tempdir)
      #write dockerfile into temp dir
      dockerfile_path = file.path(tempdir, "Dockerfile")
    }

    write(x, file = dockerfile_path)
    dockerfile_path = normalizePath(dockerfile_path)

    .built <- docker_build(
      host,
      dockerfolder = context,
      tag = image_name,
      wait = TRUE,
      no_cache = no_cache,
      dockerfile = dockerfile_path
    )
    futile.logger::flog.debug("Build output: %s\n", .built)

    if (use_workdir) {
      futile.logger::flog.info("Deleting temporary Dockerfile...")
      unlink(dockerfile_path, recursive = TRUE)
    } else {
      futile.logger::flog.info("Deleting temporary Dockerfile and directory...")
      unlink(tempdir, recursive = TRUE)
    }

  }

  return(image_name)
}


## R expression for writing sessioninfo as an object named 'info' to a given (temporary) rdata-file
.writeSessionInfoExp <- function(tempfile) {
  e1 <- quote(info <- sessionInfo())
  e2 <- quote(save(list = "info", file = tempfile))
  e2[[3]] <- tempfile
  #for debugging:
  #e3 <- quote(file.exists(tempfile))
  #e3[[2]] <- tempfile
  return(c(e1, e2))
}

#converts an vector or list of R expression into command line parmaeters for R (batch mode)
.exprToParam <-
  function(expr,
           e_append = append,
           to_string = FALSE) {
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


# Obtains a session info from a local R session executed by external system commands with the given expression expr or file
# In any case, a sessioninfo is written to a temporary file and then loaded into the current session
# If a file (R script) is given, the script is copied to a temporary file and the commands to write the sessionInfo are appended
#
# This method is used for packaging R scripts (see dockerFileFromFile)
# and for comparing session information (see test/testthat/test_sessioninfo_repoduce.R)
obtain_localSessionInfo <- function(expr = c(),
           file = NULL, # an R script to be executed
           rmd_file = NULL, # an R Markdown file
           vanilla = TRUE,
           silent = TRUE,
           slave = FALSE,
           echo = FALSE, # whether R scripts should be 'echoed'
           predetect = TRUE, # whether to use automagic to make sure all required packages are installed
           repos = "http://cloud.r-project.org",
           local_tempfile = tempfile(pattern = "rdata-sessioninfo"),
           local_temp_script = tempfile(pattern = "r-script")) {
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

  expr <- append(expr, .writeSessionInfoExp(local_tempfile))
  args <- .exprToParam(expr, to_string = TRUE)
  if (vanilla)
    args <- append("--vanilla", args)

  if (slave)
    args <- append("--slave", args)

  if (silent)
    args <- append("--silent", args)

  futile.logger::flog.info(paste("Creating an R session with the following arguments:\n\t R ",
                                 paste(args, collapse = " ")))

  system2("R", args)

  if (!file.exists(local_tempfile))
    stop("Failed to execute the script locally! A sessionInfo could not be determined.")

  load(local_tempfile)
  #clean up:
  unlink(local_tempfile)
  unlink(local_temp_script)
  return(get("info"))
}
