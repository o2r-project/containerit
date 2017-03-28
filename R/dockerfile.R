# Copyright 2017 Opening Reproducible Research (http://o2r.info)

#' dockerfile-method
#'
#' Create a Dockerfile based on either a sessionInfo, a workspace or a file
#'
#' @param from (sessionInfo, file or a string specifying the path to a workspace) The source of the information to construct the Dockerfile
#' @param save_image When TRUE, it calls \link[base]{save.image} and include the resulting .RData in the container's working directory. 
#'  Alternatively, you can pass a list of objects to be saved, which may also include arguments to be passed down to \code{save}. E.g. save_image = list("object1","object2", file = "path/in/wd/filename.RData").
#' \code{save} will be called with default arguments file = ".RData" and envir = .GlobalEnv
#' @param maintainer optionally specify the maintainer of the dockerfile. See documentation at \url{'https://docs.docker.com/engine/reference/builder/#maintainer'}
#' @param r_version (character) optionally specify the R version that should run inside the container. By default, the R version from the given sessioninfo is used (if applicable) or the version of the currently running R instance
#' @param image (From-object or character) optionally specify the image that shall be used for the docker container (FROM-statement)
#'      By default, the image is determinded from the given r_version, while the version is matched with tags from the base image rocker/r-ver
#'      see details about the rocker/r-ver at \url{'https://hub.docker.com/r/rocker/r-ver/'}
#' @param env optionally specify environment variables to be included in the image. See documentation: \url{'https://docs.docker.com/engine/reference/builder/#env}
#' @param context [TODO: CONSIDER REMOVE] (character) optionally specify a build context (path /url); the default key "workdir", assuming that the image will be build from the current working directory as returned by getwd()
#' @param soft (boolean) Whether to include soft dependencies when system dependencies are installed
#' @param copy whether and how a workspace should be copied - values: "script", "script_dir" or a list of relative file paths to be copied
#' @param container_workdir the working directory of the container
#' @param cmd The CMD statement that should be executed by default when running a parameter. Use cmd_Rscript(path) in order to reference an R script to be executed on startup
#' @param add_self Whether to add the package containeRit itself if loaded/attached to the session
#' @param vanilla Whether to use an empty vanilla session when packaging scripts and markdown files (equal to R --vanilla)
#' @param silent Whether or not to print information during execution
#' @param versioned_libs [EXPERIMENTAL] Whether it shall be attempted to match versions of linked external libraries 
#'
#' @return An object of class Dockerfile
#' @export
#' @import futile.logger
#' @examples
#' dockerfile()
#' 
#'
dockerfile <-
  function(from = utils::sessionInfo(),
           save_image = FALSE,
           maintainer = Sys.info()[["user"]],
           r_version = getRVersionTag(from),
           image = imagefromRVersion(r_version),
           env = list(generator = paste("containeRit", utils::packageVersion("containeRit"))),
           context = "workdir", 
           soft = FALSE,
           copy = "script",
           container_workdir = "/payload",
           cmd = Cmd("R"),
           add_self = FALSE,
           vanilla = TRUE,
           silent = FALSE,
           versioned_libs = FALSE
           )
      {
    if(silent){
      invisible(futile.logger::flog.threshold(futile.logger::WARN))
    }
    flog.debug("Creating a new Dockerfile from %s", from)
    .dockerfile <- NA
    .originalFrom <- class(from)

    #parse From-object from string if necessary
    if (is.character(image)) {
      image <- parseFrom(image)
    }
    
    if (is.character(maintainer)) {
      maintainer <- Label_Maintainer(maintainer)
    }

    instructions <- list()
    ### check CMD-instruction
    if(!inherits(x=cmd, "Cmd")){
      stop("Unsupported parameter for 'cmd', expected an object of class 'Cmd', given was :", class(cmd))
    }
    
    if (!inherits(x=context, "character") || (!isTRUE(context == "workdir")) && !dir.exists(context)){
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
    
    
    if (!stringr::str_detect(container_workdir, "/$"))
      # directories given as destination must have a trailing slash in dockerfiles
      container_workdir <- paste0(container_workdir, "/")

    .dockerfile <-
      new(
        "Dockerfile",
        instructions = instructions,
        maintainer = maintainer,
        image = image,
        context = context,
        cmd = cmd
      )
    
    if(is.null(from))
      #very simple case
      addInstruction(.dockerfile) <- Workdir(container_workdir)
    else if (inherits(x = from, "sessionInfo")) {
      .dockerfile <-
        dockerfileFromSession(session = from, .dockerfile = .dockerfile, soft = soft, add_self = add_self, versioned_libs = versioned_libs)
        #set the working directory (If the directory does not exist, Docker will create it)
        addInstruction(.dockerfile) <- Workdir(container_workdir)
    } else if (inherits(x = from, "character") ) {
      
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
      } 
      else {
        stop("Unsupported from. Failed to determine an existing file or directory given the following string: ", from)
      }
    } else if (is.null(from)) {
      #Creates a basic dockerfile without the 'from'-argument
      
    } else if(is.expression(from) || (is.list(from) && all(sapply(from, is.expression)))){
      #expression or list of expressions
      .sessionInfo <- clean_session(expr = from, slave = silent, vanilla = vanilla)
      .dockerfile <-
        dockerfileFromSession(
          session = .sessionInfo,
          .dockerfile = .dockerfile,
          soft = soft,
          add_self = add_self,
          versioned_libs = versioned_libs
        )
    }else {
      stop("Unsupported 'from': ", class(from)," ", from)
    }
    # copy any additional files / objects into the working directory from here:
    if(isTRUE(save_image)){
      save.image()
      addInstruction(.dockerfile) <- Copy(src = "./.RData", dest = "./")
    }else if(is.list(save_image)){
      do.call(.save_objects, save_image)
      if("file" %in% names(save_image)){
        file <- save_image$file
        # try to assure unix-compatibility..
        file <- stringr::str_replace_all(file,"\\\\","/")
      } else
        file = "./.RData"
      addInstruction(.dockerfile) <- Copy(src = file, dest = file)
    }

    flog.info("Created Dockerfile-Object based on %s", .originalFrom)
    return(.dockerfile)
  }


#' @export
toString.Dockerfile <- function(x, ...) {
  #initialize dockerfile with from
  output <- c()
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
  return(output)
}


#' @export
print.Dockerfile <- function(x, ...) {
  cat(toString.Dockerfile(x, ...), sep="\n")
  invisible(x)
}

#' @export
format.Dockerfile <- function(x, ...) format(toString(x), ...)


setMethod("format", signature(x = "Dockerfile"), format.Dockerfile)


setMethod("toString",
          signature(x = "Dockerfile"),
          toString.Dockerfile)


setMethod("as.character",
          signature(x = "Dockerfile"),
          toString.Dockerfile)

setMethod("print",
          signature(x = "Dockerfile"),
          print.Dockerfile)


.write.Dockerfile <-
  function(x, file = file.path(.contextPath(x), "Dockerfile")) {
    flog.info("Writing dockerfile to %s", file)
    return(write(toString(x), file))
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



dockerfileFromSession <- function(session, .dockerfile, soft, add_self, versioned_libs) {
  instructions <- slot(.dockerfile, "instructions")

  apks <- session$otherPkgs
  lpks <- session$loadedOnly
  pkgs <- append(apks, lpks) ##packages to be installed
  if(!add_self)
    pkgs <- pkgs[names(pkgs) != "containeRit"]

  # The platform is determined only from kown images. Alternatively, we could let the user optionally specify one amongst different supported platforms
  platform = NULL
  image_name = .dockerfile@image@image
  if(image_name %in% .rocker_images)
    platform = .debian_platform

  .dockerfile <- .create_run_install(.dockerfile = .dockerfile, pkgs = pkgs, platform = platform, soft = soft, versioned_libs = versioned_libs)

  return(.dockerfile)
}

dockerfileFromFile <- function(file, .dockerfile, soft, copy, add_self, copy_destination, vanilla, silent, versioned_libs) {
  #################################################
  # prepare context and normalize paths:
  #################################################
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
  ####################################################
  # execute script / markdowns and obtain sessioninfo
  #####################################################
  if(stringr::str_detect(file, ".R$")){
    message <- paste0("Executing R script file in ", rel_path," locally.")
    futile.logger::flog.info(message)
    sessionInfo <- obtain_localSessionInfo(file = file, vanilla = vanilla, slave = silent)
  } else if(stringr::str_detect(file, ".Rnw$")){
    message <- paste0("Processing the given file ", rel_path," locally using knitr::knit2pdf(..., clean = TRUE)")
    futile.logger::flog.info(message)
    sessionInfo <- obtain_localSessionInfo(rnw_file = file, vanilla = vanilla , slave = silent)
  }else if(stringr::str_detect(file, ".Rmd$")){
    message <- paste0("Processing the given file ", rel_path," locally using rmarkdown::render(...)")
    futile.logger::flog.info(message)
    sessionInfo <- obtain_localSessionInfo(rmd_file = file, vanilla = vanilla, slave = silent)
  } else{
    message <- paste0("The supplied file ", rel_path, " has no known extension. ContaineRit will handle it as an R script for packaging.")
    futile.logger::flog.info(message)
  }
  # append system dependencies and package installation instructions
  ####################################################
  .dockerfile <- dockerfileFromSession(session = sessionInfo, .dockerfile = .dockerfile, soft = soft, add_self = add_self,
                                       versioned_libs = versioned_libs)
  
  ## set working directory to the copy destination and add copy instructions
  ####################################################
  addInstruction(.dockerfile) <- Workdir(copy_destination)
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
  

  return(.dockerfile)
}


dockerfileFromWorkspace <-
  function(path, .dockerfile, soft, add_self, copy, copy_destination, vanilla, silent, versioned_libs) {
    
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
    
    if (length(.rFiles) > 0) {
      if (length(.rFiles) > 1)
        warning(
          "Found ",
          length(.rFiles),
          " .R files in the workspace. ContaineRit will use the first one as follows for packaging: \n\t",
          .rFiles[1]
        )
      else{
        message <- paste0("ContaineRit will use the following R script for packaging: \n\t",
                .rFiles[1])
        futile.logger::flog.info(message)
      }
      
      target_file <- .rFiles[1]

    } else if (length(.md_Files) > 0) {
      if (length(.md_Files) > 1)
        warning(
          "Found ",
          length(.md_Files),
          " Sweave / Markdown files in the workspace. ContaineRit will use the first one as follows for packaging: \n\t",
          .md_Files[1]
        )
      else{
        message <- paste0(
          "ContaineRit will use the following Sweave / Markdown file for packaging: \n\t",
          .md_Files[1]
        )
        futile.logger::flog.info(message)
      }
      target_file <- .md_Files[1]
    } else
      stop("The Workspace does not contain any R file that can be packaged.")
    
    return(
      dockerfileFromFile(
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
    )
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
    if(stringr::str_length(rel_path)==0)
      rel_path <- "."
    return(rel_path)
  })
  as.character(out)
}

# helper function for saving lists of objects
.save_objects <- function(... , file = ".RData", envir = .GlobalEnv){
  save(..., file = file, envir = envir) 
}


