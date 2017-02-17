# Copyright 2016 Opening Reproducible Research (http://o2r.info)

#' dockerfile-method
#'
#' Create a Dockerfile based on either a sessionInfo, a workspace or a file
#'
#' @param from (sessionInfo, file or a string specifying the path to a workspace) The source of the information to construct the Dockerfile
#' @param objects character vector naming all R objects to be included in the docker image / R session. Can be character(0) (default), ls() or a fraction of ls()
#' @param maintainer optionally specify the maintainer of the dockerfile. See the \code{Maintainter-class} and the official documentation: \url{'https://docs.docker.com/engine/reference/builder/#maintainer'}
#' @param r_version (character) optionally specify the R version that should run inside the container. By default, the R version from the given sessioninfo is used (if applicable) or the version of the currently running R instance
#' @param image (From-object or character) optionally specify the image that shall be used for the docker container (FROM-statement)
#'      By default, the image is determinded from the given r_version, while the version is matched with tags from the base image rocker/r-ver
#'      see details about the rocker/r-ver at \url{'https://hub.docker.com/r/rocker/r-ver/'}
#' @param env optionally specify environment variables to be included in the image. See documentation: \url{'https://docs.docker.com/engine/reference/builder/#env}
#' @param context (character) optionally specify a build context (path /url); the default key "workdir", assuming that the image will be build from the current working directory as returned by getwd()
#' @param soft (boolean) Whether to include soft dependencies when system dependencies are installed
#' @param copy whether and how a workspace should be copied - values: "script", "script_dir" or a list of relative file paths to be copied
#' @param container_workdir the working directory of the container
#' @param cmd The CMD statement that should be executed by default when running a parameter. Use cmd_Rscript(path) in order to reference an R script to be executed on startup
#' @param add_self Whether to add the package containeRit itself if loaded/attached to the session
#'
#' @return An object of class Dockerfile
#' @export
#' @import futile.logger
#' @examples
#' dockerfile()
#'
dockerfile <-
  function(from = utils::sessionInfo(),
           objects = character(0),
           maintainer = Maintainer(name = Sys.info()[["user"]]),
           r_version = getRVersionTag(from),
           image = imagefromRVersion(r_version),
           env = list(generator = paste("containeRit", utils::packageVersion("containeRit"))),
           context = "workdir", 
           soft = FALSE,
           copy = "script",
           container_workdir = "payload/",
           cmd = Cmd("R"),
           add_self = FALSE
           )
      {
    flog.debug("Creating a new Dockerfile from %s", from)
    .dockerfile <- NA
    .originalFrom <- class(from)

    #parse From-object from string if necessary
    if (is.character(image)) {
      image <- parseFrom(image)
    }

    instructions <- list()
    ### check CMD-instruction
    if(!inherits(x=cmd, "Cmd")){
      stop("Unsupported parameter for 'cmd', expected an object of class 'Cmd', given was :", class(cmd))
    }
    
    if(!inherits(x=context, "character") || (!isTRUE(context == "workdir")) && !dir.exists(context)){
      stop("Unsupported parameter for 'context', expected an existing directory path or the the key 'workdir', given was :", class(context)," ",context)
    }
    
    # whether image is supported
    image_name <- image@image
    if (!image_name %in% .supported_images) {
      stop(
        "Invalid base image. Currently, only the following base images are supported: ",
        paste(.supported_images, collapse = "\n")
      )
    }

    .dockerfile <-
      new(
        "Dockerfile",
        instructions = instructions,
        maintainer = maintainer,
        image = image,
        context = context,
        cmd = cmd
      )
    
    #set the working directory (If the directory does not exist, Docker will create it)
    addInstruction(.dockerfile) <- Workdir(container_workdir)

    if (inherits(x = from, "sessionInfo")) {
      .dockerfile <-
        dockerfileFromSession(session = from, .dockerfile = .dockerfile, soft = soft, add_self = add_self)
    } else if (inherits(x = from, "character") ) {
      
      if (dir.exists(from)){
        .originalFrom <- from
        .dockerfile <-
          dockerfileFromWorkspace(path = from, .dockerfile = .dockerfile, soft = soft, add_self = add_self)
      } else if (file.exists(from)){
        .originalFrom <- from
        .dockerfile <-
          dockerfileFromFile(file = from, .dockerfile = .dockerfile, soft = soft, copy = copy, add_self = add_self)
      } else {
        stop("Unsupported from. Failed to determine an existing file or directory given the following string: ", from)
      }
    } else if (is.null(from)) {
      #Creates a basic dockerfile without the 'from'-argument
    } else {
      stop("Unsupported 'from': ", class(from)," ", from)
    }

    flog.info("Created Dockerfile-Object based on %s", .originalFrom)
    message("Created Dockerfile-Object based on ", .originalFrom, ".")
    return(.dockerfile)
  }

#' Format a Dockerfile object to a series of instructions
#'
#' @param x An object of class Dockerfile
#' @param ... Arguments to be passed down to format.default
#'
#' @return The content of the Dockerfile represented by the Dockerfile object, by default formatted as a list of strings where each string represent a new line
#' @export
#'
#' @examples
#' format(dockerfile())
format.Dockerfile <- function(x, ...) {
  #initialize dockerfile with from
  output <- list()
  from <- toString(slot(x, "image"))
  output <- append(output, from)
  maintainer <- slot(x, "maintainer")
  if (!is.null(maintainer))
    output <- append(output, toString(maintainer))
  instructions <- slot(x, "instructions")
  if (!is.null(instructions) && length(instructions) > 0) {
    instructions <- sapply(instructions, toString)
    output <- append(output, unlist(instructions))
  }
  cmd <- slot(x, "cmd")
  if (!is.null(cmd))
    output <- append(output, toString(cmd))
  return(format(output, ...))
}



.write.Dockerfile <-
  function(x, file = file.path(.contextPath(x), "Dockerfile")) {
    flog.info("Writing dockerfile to %s", file)
    return(write(as.character(format(x)), file))
  }


#' Write a Dockerfile object to a dockerfile
#'
#' @param x Dockerfile object to be serialized
#' @param file optional argument specifying a costum file path
#'
#' @export
#'
#' @examples
#' # write a dockerfile with default parameters to temporary file and show content:
#' temp = tempfile()
#' write(dockerfile(), file=temp)
#' print(readLines(temp))
#' unlink(temp)
#' 
setMethod("write", signature(x = "Dockerfile"), .write.Dockerfile)



dockerfileFromSession <- function(session, .dockerfile, soft, add_self) {
  instructions <- slot(.dockerfile, "instructions")

  apks <- session$otherPkgs
  lpks <- session$loadedOnly
  pkgs <- append(apks, lpks) ##packages to be installed

  # The platform is determined only from kown images. Alternatively, we could let the user optionally specify one amongst different supported platforms
  platform = NULL
  image_name = .dockerfile@image@image
  if(image_name %in% .rocker_images)
    platform = .debian_platform

  run_instructions <- .create_run_install(pkgs, platform = platform, soft = soft, add_self = add_self)

  instructions <- append(instructions, run_instructions)
  slot(.dockerfile, "instructions") <- instructions
  return(.dockerfile)
}

dockerfileFromFile <- function(file, .dockerfile, soft, copy, add_self) {
  context = .contextPath(.dockerfile)
  file = normalizePath(file)

  #Is the file within the context?
  len = stringr::str_length(context)
  substr = stringr::str_sub(context, end=len)
  if(context != substr)
    stop("The given file is not inside the context directory!")

  #If context directory and work directory are not the same, there might occur problems with relative paths
  if(context != getwd())
    warning("The context directory is not the same as the current R working directory! Code/workspace may not be reproducible.")
  # make sure that the path is relative to context
  rel_path <- .makeRelative(file, context)
  
  copy = unlist(copy)
  if(!is.character(copy)){
    stop("Invalid argument given for 'copy'")
  } else if(length(copy) == 1 && copy == "script"){
    #unless we use some kind of Windows-based docker images, the destination path has to be unix compatible:
    rel_path_dest <- stringr::str_replace_all(rel_path,pattern = "\\\\",replacement="/")
    addInstruction(.dockerfile) <- Copy(rel_path, rel_path_dest)
  }else if(length(copy) == 1 && copy == "script_dir"){
    script_dir <- normalizePath(dirname(file))
    rel_dir <- .makeRelative(script_dir, context)

    #unless we use some kind of Windows-based docker images, the destination path has to be unix compatible:
    rel_dir_dest <- stringr::str_replace_all(rel_dir,pattern = "\\\\",replacement="/")
    if (!stringr::str_detect(rel_dir_dest, "/$"))
      # directories given as destination must have a trailing slash in dockerfiles
      rel_dir_dest <- paste0(rel_dir_dest, "/")

    addInstruction(.dockerfile) <- Copy(rel_dir, rel_dir_dest)
  }else {
    ## assume that a list or vector of paths is given
    sapply(copy, function(file){
      if(file.exists(file)){
        rel_path <- .makeRelative(normalizePath(file), context)
        rel_path_dest <- stringr::str_replace_all(rel_path,pattern = "\\\\",replacement="/")
        if(dir.exists(file) && !stringr::str_detect(rel_path_dest, "/$"))
            rel_path_dest <- paste0(rel_dir_dest, "/")
        addInstruction(.dockerfile) <<- Copy(rel_path, rel_path_dest)
      } else {
        stop("The file ", file, ", given by 'copy', does not exist! Invalid argument.")
      }
    }
    )
  }
  
  if(stringr::str_detect(file, ".R$")){
    message("Executing R script file in ", rel_path," locally.")
    sessionInfo <- obtain_localSessionInfo(file = file)
  } else if(stringr::str_detect(file, ".Rnw$")){
      message("Processing the given file ", rel_path," locally using knitr::knit2pdf(..., clean = TRUE)")
      sessionInfo <- obtain_localSessionInfo(rnw_file = file)
  }else if(stringr::str_detect(file, ".Rmd$")){
    message("Processing the given file ", rel_path," locally using rmarkdown::render(...)")
    sessionInfo <- obtain_localSessionInfo(rmd_file = file)
  } else
    message("The supplied file ", rel_path, " has no known extension. ContaineRit will handle it as an R script for packaging.")
  
  ##append system dependencies
  .dockerfile <- dockerfileFromSession(session = sessionInfo, .dockerfile = .dockerfile, soft = soft, add_self = add_self)
    
  return(.dockerfile)
}


dockerfileFromWorkspace <-
  function(path, .dockerfile, soft, add_self) {
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
    
    if (length(.rFiles) > 0) {
      if (length(.rFiles) > 1)
        warning(
          "Found ",
          length(.rFiles),
          " .R files in the workspace. ContaineRit will use the first one as follows for packaging: \n\t",
          .rFiles[1]
        )
      else
        message("ContaineRit will use the following R script for packaging: \n\t",
                .rFiles[1])
      
      return(
        dockerfileFromFile(
          .rFiles[1],
          .dockerfile = .dockerfile,
          soft = soft,
          copy = "script_dir",
          add_self = add_self
        )
      )
    } else if (length(.md_Files) > 0) {
      if (length(.md_Files) > 1)
        warning(
          "Found ",
          length(.md_Files),
          " Sweave / Markdown files in the workspace. ContaineRit will use the first one as follows for packaging: \n\t",
          .md_Files[1]
        )
      else
        message(
          "ContaineRit will use the following Sweave / Markdown file for packaging: \n\t",
          .md_Files[1]
        )
      
      return(
        dockerfileFromFile(
          .md_Files[1],
          .dockerfile = .dockerfile,
          soft = soft,
          copy = "script_dir",
          add_self = add_self
        )
      )
      
    } else
      stop("The Workspace does not contain any R file that can be packaged.")
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

#' Get R version from a variety of sources in a string format used for image tags
#'
#' Returns either a version extracted from a given object or the default version.
#'
#' @param from the source to extract an R version: a `sessionInfo()` object
#' @param default if 'from' does not contain version information (e.g. its an Rscript), use this default version information.
#' 
#' @export
#'
#' @examples
#' getRVersionTag(from = sessionInfo())
#' getRVersionTag()
getRVersionTag <- function(from = NULL, default = R.Version()) {
  r_version <- NULL
  if (inherits(from, "sessionInfo")) {
    r_version <- from$R.version
  } else
    r_version <- default

  return(paste(r_version$major, r_version$minor, sep = "."))
}

.makeRelative <- function(files, from) {
  out = sapply(files, function(file) {
    len = stringr::str_length(from)
    rel_path = stringr::str_sub(file, start = len + 1)
    if (stringr::str_detect(rel_path, "^[\\/]"))
      rel_path = stringr::str_sub(rel_path, start = 2)
    if(stringr::str_length(rel_path)==0)
      rel_path <- "."
    return(rel_path)
  })
  as.character(out)
}

