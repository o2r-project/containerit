# Copyright 2017 Opening Reproducible Research (http://o2r.info)

#' dockerfile-method
#'
#' Create a Dockerfile based on either a sessionInfo, a workspace or a file.
#'
#' @section Based on \code{sessionInfo}:
#'
#' Use the current \code{\link[utils]{sessionInfo})} to create a Dockerfile.
#'
#' @section Based on a workspace/directory:
#'
#' Given an existing path to a directory, the method tries to automatically find the main \code{R} file within that directory.
#' Files are searched recursively. The following types are supported:
#'
#' \enumerate{
#'   \item regular R script files, identified by file names ending in \code{.R}
#'   \item weaved documents, identified by file names ending in either \code{.Rmd} or \code{.Rnw}
#' }
#'
#' After identifying the main file, the process continues as described in the section file.
#' If both types are found, documents are given priority over scripts.
#' If multiple files are found, the first file as returned by \code{\link[base]{dir}} will be used.
#'
#' @section Based on a file:
#'
#' Given an executable \code{R} script or document, create a Dockerfile to execute this file.
#' This executes the whole file to obtain a complete \code{sessionInfo} object, see section "Based on \code{sessionInfo}", and copies required files and documents into the container.
#'
#' @param from The source of the information to construct the Dockerfile. Can be a \code{sessionInfo} object, a path to a file, or the path to a workspace).
#' @param save_image When TRUE, it calls \link[base]{save.image} and include the resulting .RData in the container's working directory.
#'  Alternatively, you can pass a list of objects to be saved, which may also include arguments to be passed down to \code{save}. E.g. save_image = list("object1","object2", file = "path/in/wd/filename.RData").
#' \code{save} will be called with default arguments file = ".RData" and envir = .GlobalEnv
#' @param maintainer optionally specify the maintainer of the dockerfile. See documentation at \url{'https://docs.docker.com/engine/reference/builder/#maintainer'}. Defaults to \code{Sys.info()[["user"]]}.
#' @param r_version (character) optionally specify the R version that should run inside the container. By default, the R version from the given sessioninfo is used (if applicable) or the version of the currently running R instance
#' @param image (From-object or character) optionally specify the image that shall be used for the Docker container (FROM-statement)
#'      By default, the image is determinded from the given r_version, while the version is matched with tags from the base image rocker/r-ver
#'      see details about the rocker/r-ver at \url{'https://hub.docker.com/r/rocker/r-ver/'}
#' @param env optionally specify environment variables to be included in the image. See documentation: \url{'https://docs.docker.com/engine/reference/builder/#env}
#' @param soft (boolean) Whether to include soft dependencies when system dependencies are installed
#' @param copy whether and how a workspace should be copied - values: "script", "script_dir" or a list of relative file paths to be copied
#' @param container_workdir the working directory of the container
#' @param cmd The CMD statement that should be executed by default when running a parameter. Use cmd_Rscript(path) in order to reference an R script to be executed on startup
#' @param add_self Whether to add the package containerit itself if loaded/attached to the session
#' @param vanilla Whether to use an empty vanilla session when packaging scripts and markdown files (equal to R --vanilla)
#' @param silent Whether or not to print information during execution
#' @param versioned_libs [EXPERIMENTAL] Whether it shall be attempted to match versions of linked external libraries
#'
#' @return An object of class Dockerfile
#' @export
#' @import futile.logger
#' @examples
#' dockerfile <- dockerfile()
#' print(dockerfile)
#'
dockerfile <-
  function(from = utils::sessionInfo(),
           save_image = FALSE,
           maintainer = Sys.info()[["user"]],
           r_version = getRVersionTag(from),
           image = imagefromRVersion(r_version),
           env = list(generator = paste("containerit", utils::packageVersion("containerit"))),
           soft = FALSE,
           copy = "script",
           container_workdir = "/payload",
           cmd = Cmd("R"),
           add_self = FALSE,
           vanilla = TRUE,
           silent = FALSE,
           versioned_libs = FALSE)
  {
    if (silent) {
      invisible(futile.logger::flog.threshold(futile.logger::WARN))
    }

    futile.logger::flog.debug("Creating a new Dockerfile from '%s'", from)
    .dockerfile <- NA
    .originalFrom <- class(from)

    #parse From-object from string if necessary
    if (is.character(image)) {
      image <- parseFrom(image)
    }

    if (is.character(maintainer)) {
      .label <- Label_Maintainer(maintainer)
      futile.logger::flog.debug("Turning maintainer character string '%s' into label: %s", maintainer, toString(.label))
      maintainer <- .label
    }

    # check CMD-instruction
    instructions <- list()
    if (!inherits(x = cmd, "Cmd")) {
      stop(
        "Unsupported parameter for 'cmd', expected an object of class 'Cmd', given was :",
        class(cmd)
      )
    }

    # whether image is supported
    image_name <- image@image
    if (!image_name %in% .supported_images) {
      warning(
        "Unsupported base image. Proceed at your own risk. The following base images are supported:\n",
        paste(.supported_images, collapse = "\n")
      )
    }

    if (!stringr::str_detect(container_workdir, "/$")) {
      # directories given as destination must have a trailing slash in dockerfiles
      container_workdir <- paste0(container_workdir, "/")
      futile.logger::flog.debug("Appended trailing slash, workdir is '%s'", container_workdir)
    }

    .dockerfile <-
      new(
        "Dockerfile",
        instructions = instructions,
        maintainer = maintainer,
        image = image,
        cmd = cmd
      )

    if (is.null(from)) {
      addInstruction(.dockerfile) <- Workdir(container_workdir)
    } else if (inherits(x = from, "sessionInfo")) {
      .dockerfile <-
        dockerfileFromSession(
          session = from,
          .dockerfile = .dockerfile,
          soft = soft,
          add_self = add_self,
          versioned_libs = versioned_libs
        )
      #set the working directory (If the directory does not exist, Docker will create it)
      addInstruction(.dockerfile) <- Workdir(container_workdir)
    } else if (inherits(x = from, "character")) {
      futile.logger::flog.debug("Creating from character string")

      if (dir.exists(from)) {
        .originalFrom <- from
        .dockerfile <-
          dockerfileFromWorkspace(
            path = from,
            .dockerfile = .dockerfile,
            soft = soft,
            add_self = add_self,
            copy = copy,
            copy_destination = container_workdir ,
            vanilla = vanilla,
            silent = silent,
            versioned_libs = versioned_libs
          )
      } else if (file.exists(from)) {
        .originalFrom <- from
        .dockerfile <-
          dockerfileFromFile(
            file = from,
            .dockerfile = .dockerfile,
            soft = soft,
            add_self = add_self,
            copy = copy,
            copy_destination = container_workdir,
            vanilla = vanilla,
            silent = silent,
            versioned_libs = versioned_libs
          )
      } else {
        stop(
          "Unsupported argument 'from'. Failed to determine an existing file or directory given the string: ",
          from
        )
      }
    } else if (is.null(from)) {
      #Creates a basic dockerfile without the 'from'-argument

    } else if (is.expression(from) ||
               (is.list(from) && all(sapply(from, is.expression)))) {
      #expression or list of expressions
      .sessionInfo <-
        clean_session(expr = from,
                      slave = silent,
                      vanilla = vanilla)
      .dockerfile <-
        dockerfileFromSession(
          session = .sessionInfo,
          .dockerfile = .dockerfile,
          soft = soft,
          add_self = add_self,
          versioned_libs = versioned_libs
        )
    } else {
      stop("Unsupported 'from': ", class(from), " ", from)
    }

    # copy any additional files / objects into the working directory from here:
    if (isTRUE(save_image)) {
      save.image()
      addInstruction(.dockerfile) <-
        Copy(src = "./.RData", dest = "./")
    } else if (is.list(save_image)) {
      do.call(.save_objects, save_image)
      if ("file" %in% names(save_image)) {
        file <- save_image$file
        # try to assure unix-compatibility..
        file <- stringr::str_replace_all(file, "\\\\", "/")
      } else
        file = "./payload.RData"
      addInstruction(.dockerfile) <- Copy(src = file, dest = file)
    }

    flog.info("Created Dockerfile-Object based on %s", .originalFrom)
    return(.dockerfile)
  }

dockerfileFromSession <-
  function(session,
           .dockerfile,
           soft,
           add_self,
           versioned_libs) {
    futile.logger::flog.debug("Creating from sessionInfo")
    instructions <- slot(.dockerfile, "instructions")

    apks <- session$otherPkgs
    lpks <- session$loadedOnly
    pkgs <- append(apks, lpks) ##packages to be installed
    if (!add_self)
      pkgs <- pkgs[names(pkgs) != "containerit"]

    # The platform is determined only from kown images. Alternatively, we could let the user optionally specify one amongst different supported platforms
    platform = NULL
    image_name = .dockerfile@image@image
    if (image_name %in% .rocker_images)
      platform = .debian_platform

    .dockerfile <-
      .create_run_install(
        .dockerfile = .dockerfile,
        pkgs = pkgs,
        platform = platform,
        soft = soft,
        versioned_libs = versioned_libs
      )

    return(.dockerfile)
  }

dockerfileFromFile <-
  function(file,
           .dockerfile,
           soft,
           copy,
           add_self,
           copy_destination,
           vanilla,
           silent,
           versioned_libs) {
    futile.logger::flog.debug("Creating from file")
    #################################################
    # prepare context ( = working directory) and normalize paths:
    #################################################
    context = normalizePath(getwd())
    file = normalizePath(file)

    #Is the file within the context?
    len = stringr::str_length(context)
    substr = stringr::str_sub(context, end = len)
    if (context != substr)
      stop("The given file is not inside the context directory!")

    # make sure that the path is relative to context
    rel_path <- .makeRelative(file, context)

    ####################################################
    # execute script / markdowns and obtain sessioninfo
    #####################################################
    if (stringr::str_detect(file, ".R$")) {
      futile.logger::flog.info("Executing R script file in %s locally.", rel_path)
      sessionInfo <-
        obtain_localSessionInfo(
          file = file,
          vanilla = vanilla,
          slave = silent,
          echo = !silent
        )
    } else if (stringr::str_detect(file, ".Rnw$")) {
      futile.logger::flog.info("Processing the given file %s locally using knitr::knit2pdf(..., clean = TRUE)", rel_path)
      sessionInfo <-
        obtain_localSessionInfo(
          rnw_file = file,
          vanilla = vanilla ,
          slave = silent,
          echo = !silent
        )
    } else if (stringr::str_detect(file, ".Rmd$")) {
      futile.logger::flog.info("Processing the given file %s locally using rmarkdown::render(...)", rel_path)
      sessionInfo <-
        obtain_localSessionInfo(
          rmd_file = file,
          vanilla = vanilla,
          slave = silent,
          echo = !silent
        )
    } else{
      futile.logger::flog.info("The supplied file %s has no known extension. containerit will handle it as an R script for packaging.", rel_path)
    }

    ####################################################
    # append system dependencies and package installation instructions
    ####################################################
    .dockerfile <-
      dockerfileFromSession(
        session = sessionInfo,
        .dockerfile = .dockerfile,
        soft = soft,
        add_self = add_self,
        versioned_libs = versioned_libs
      )

    ## set working directory to the copy destination and add copy instructions
    ####################################################
    addInstruction(.dockerfile) <- Workdir(copy_destination)
    copy = unlist(copy)
    if (!is.character(copy)) {
      stop("Invalid argument given for 'copy'")
    } else if (length(copy) == 1 && copy == "script") {
      #unless we use some kind of Windows-based Docker images, the destination path has to be unix compatible:
      rel_path_dest <-
        stringr::str_replace_all(rel_path, pattern = "\\\\", replacement = "/")
      addInstruction(.dockerfile) <- Copy(rel_path, rel_path_dest)
    } else if (length(copy) == 1 && copy == "script_dir") {
      script_dir <- normalizePath(dirname(file))
      rel_dir <- .makeRelative(script_dir, context)

      #unless we use some kind of Windows-based Docker images, the destination path has to be unix compatible:
      rel_dir_dest <-
        stringr::str_replace_all(rel_dir, pattern = "\\\\", replacement = "/")
      if (!stringr::str_detect(rel_dir_dest, "/$"))
        # directories given as destination must have a trailing slash in dockerfiles
        rel_dir_dest <- paste0(rel_dir_dest, "/")

      addInstruction(.dockerfile) <- Copy(rel_dir, rel_dir_dest)
    } else {
      ## assume that a list or vector of paths is given
      sapply(copy, function(file) {
        if (file.exists(file)) {
          rel_path <- .makeRelative(normalizePath(file), context)
          rel_path_dest <-
            stringr::str_replace_all(rel_path, pattern = "\\\\", replacement = "/")
          if (dir.exists(file) &&
              !stringr::str_detect(rel_path_dest, "/$"))
            rel_path_dest <- paste0(rel_dir_dest, "/")
          addInstruction(.dockerfile) <<-
            Copy(rel_path, rel_path_dest)
        } else {
          stop("The file ",
               file,
               ", given by 'copy', does not exist! Invalid argument.")
        }
      })
    }


    return(.dockerfile)
  }

dockerfileFromWorkspace <-
  function(path,
           .dockerfile,
           soft,
           add_self,
           copy,
           copy_destination,
           vanilla,
           silent,
           versioned_libs) {
    futile.logger::flog.debug("Creating from workspace directory")
    target_file <- NULL #file to be packaged

    .rFiles <-
      dir(
        path = path,
        pattern = "\\.R$",
        full.names = TRUE,
        include.dirs = FALSE,
        recursive = TRUE
      )

    .md_Files <-
      dir(
        path = path,
        pattern = "\\.Rmd$|\\.Rnw$",
        full.names = TRUE,
        include.dirs = FALSE,
        recursive = TRUE
      )
    futile.logger::flog.debug("Found %s scripts and %s documents", length(.rFiles), length(.md_Files))

    if (length(.rFiles) > 0 && length(.md_Files) > 0) {
      target_file <- .md_Files[1]
      warning("Found both scripts and weaved documents (Rmd, Rnw) in the given directory. Using the first document for packaging: \n\t",
              target_file
      )
    } else if (length(.md_Files) > 0) {
      target_file <- .md_Files[1]
      if (length(.md_Files) > 1)
        warning("Found ", length(.md_Files), " document files in the workspace, using '", target_file, "'")
    } else if (length(.rFiles) > 0) {
      target_file <- .rFiles[1]
      if (length(.rFiles) > 1)
        warning("Found ", length(.rFiles), " script files in the workspace, using '", target_file, "'")
    }

    if (is.null(target_file))
      stop("Workspace does not contain any R file that can be packaged.")
    else
      futile.logger::flog.info("Found file for packaging in workspace: %s", target_file)

    .df <- dockerfileFromFile(
      target_file,
      .dockerfile = .dockerfile,
      soft = soft,
      copy = copy,
      add_self = add_self,
      copy_destination = copy_destination,
      vanilla = vanilla,
      silent = silent,
      versioned_libs = versioned_libs
    )
    return(.df)
  }

imagefromRVersion <- function(r_version) {
  #check if dockerized R version is available (maybe check other repositories too?)
  tags <- tagsfromRemoteImage(.rocker_images[["versioned"]])
  if (!r_version %in% tags) {
    warning(
      "No Docker image found for the given R version. ",
      "You might want to specify a custom Docker image or \n",
      "  use one of the following supported version tags ",
      "(maybe check the internet connection if no suggestions appear). \n\t",
      paste(tags, collapse = " ")
    )
  }

  image <- From(.rocker_images[["versioned"]], tag = r_version)
  return(image)
}

tagsfromRemoteImage <- function(image) {
  urlstr <-
    paste0("https://registry.hub.docker.com/v2/repositories/",
           image,
           "/tags/")

  tryCatch({
    con <- url(urlstr)
    str <- readLines(con, warn = FALSE)
  },
  finally = close(con))

  parser <- rjson::newJSONParser()
  parser$addData(str)
  tags <- sapply(parser$getObject()$results, function(x) {
    x$name
  })
  return(tags)
}

.makeRelative <- function(files, from) {
  out = sapply(files, function(file) {
    len = stringr::str_length(from)
    rel_path = stringr::str_sub(file, start = len + 1)
    if (stringr::str_detect(rel_path, "^[\\/]"))
      rel_path = stringr::str_sub(rel_path, start = 2)
    if (stringr::str_length(rel_path) == 0)
      rel_path <- "."
    return(rel_path)
  })
  as.character(out)
}

# helper function for saving lists of objects
.save_objects <-
  function(... , file = ".RData", envir = .GlobalEnv) {
    save(..., file = file, envir = envir)
  }
