#' Build a Docker image from a local Dockerfile
#'
#' Uploads a folder with a Dockerfile and supporting files to an instance and builds it.
#' The method is implemented based on \code{harbor::docker_cmd} and analogue to \code{googleComputeEngineR::docker_build} (with small differences)
#' 
#' @param host A host object (see harbor-package)
#' @param dockerfolder Local location of build directory including valid Dockerfile
#' @param new_image Name of the new image to be created
#' @param wait Whether to block R console until finished build
#' @param no_cache Wheter to use cached layers to build the image
#' @param ... Other arguments passed to the SSH command for the host
#'
#' @return A table of active images on the instance
#' @export
docker_build <-
  function (host = harbor::localhost,
            dockerfolder,
            new_image,
            wait = FALSE,
            no_cache = FALSE,
            ...) {
    #TODO: This method may be enhanced with random image name as default (?) 
    # and also handle Dockerfile-Objects as input, analogue to the internal method 'create_localDockerImage'
    stopifnot(file.exists(dockerfolder))
    docker_opts <- paste("-t", new_image)
    if (no_cache)
      docker_opts <- append(docker_opts, "--no-cache")
    
    harbor::docker_cmd(
      host,
      "build",
      args = dockerfolder,
      docker_opts = docker_opts,
      wait = wait,
      ...
    )
    
    harbor::docker_cmd(host, "images", ..., capture_text = TRUE)
  }

#shorthand method for creating a local docker Image based on either an existing dockerfile (given by folder) or a Dockerfile object
create_localDockerImage <- function(x, host = harbor::localhost,
                                   image_name = strsplit(tempfile(pattern = "containerit_test", tmpdir = ""), "/")[[1]][2],
                                   no_cache = FALSE) {
  if (is.character(x))
    docker_build(
      harbor::localhost,
      dockerfolder = x,
      new_image = image_name,
      wait = TRUE
    )
  if (inherits(x, "Dockerfile")) {
    message("Building docker image from temporary docker file...")
    tempdir <- tempfile(pattern = "dir")
    dir.create(tempdir)
    #write dockerfile into temp dir
    write(x, file = file.path(tempdir, "Dockerfile"))
    docker_build(
      host,
      dockerfolder = tempdir,
      new_image = image_name,
      wait = TRUE,
      no_cache = no_cache
    )
    message("Deleting temporary docker file...")
    unlink(tempdir, recursive = TRUE)
    
  }
  return(image_name)
}


## R expression for writing sessioninfo as an object named 'info' to a given (temporary) rdata-file
.writeSessionInfoExp <- function(tempfile) {
  e1 <- quote(info <- sessionInfo())
  e2 <- quote(save(list = "info", file = tempfile))
  e2[[3]] <- tempfile
  
  return(c(e1, e2))
}

#converts an vector or list of R expression into commandline parmaeters for RScript
.exprToParam <- function(expr) {
  #convert from expressions to enquoted strings
  expr <- sapply(expr, deparse)
  expr <- sapply(expr, deparse)
  expr <- sapply(expr, function(x) {
    paste("-e", x)
  }, simplify = TRUE, USE.NAMES = FALSE)
  return(expr)
}

##optains a session info from a local R session executed by external system commands with the given expression expr
obtain_localSessionInfo <-
  function(expr = c(),
           vanilla = FALSE,
           local_tempfile = tempfile(pattern = "rdata-sessioninfo")) {
    #create a local sessionInfo
    expr <- append(expr, .writeSessionInfoExp(local_tempfile))
    #convert to cmd parameters
    expr <- .exprToParam(expr)
    
    if (vanilla)
      expr <- append("--vanilla", expr)
    
    message(
      "Creating R session with the following arguments:\n\t Rscript ",
      paste(expr, collapse = " ")
    )
    system2("Rscript", expr)
    load(local_tempfile)
    #unlink(local_tempfile);rm(local_tempfile)
    return(get("info"))
  }

##optains a session info from an R session executed in docker given expression expr and a docker image with R installed
obtain_dockerSessionInfo <-
  function(docker_image,
           expr = c(),
           vanilla = FALSE,
           docker_tempdir = "/tmp/containerit_temp",
           local_tempdir = tempfile(pattern = "dir")
           ,
           deleteTempfiles = TRUE) {
    #create local temporary directory
    dir.create(local_tempdir)
    if (!dir.exists(local_tempdir))
      stop("Unable to locate temporary directory: ", local_tempdir)
    
    #mount option
    volume_opt = paste0("-v ", local_tempdir, ":", docker_tempdir)
    
    #rdata file to which session info shall be written
    docker_tempfile =  paste0(docker_tempdir, "/", "rdata")
    local_docker_tempfile = file.path(local_tempdir, "rdata")
    #cat(writeExp(docker_tempfile))
    expr <- append(expr, .writeSessionInfoExp(docker_tempfile))
    #convert to cmd parameters
    expr <- .exprToParam(expr)
    
    cmd <- c("Rscript")
    if (vanilla) {
      cmd <- append(cmd, "--vanilla")
    }
    cmd <- append(cmd, expr)
    message("Creating R session in Docker with the following arguments:\n\t",
            cmd)
    container <- harbor::docker_run(
      harbor::localhost,
      image = docker_image,
      cmd = cmd ,
      docker_opts = volume_opt
    )
    
    if (harbor::container_running(container))
      stop("Unexpected behavior: The container is still running!")
    
    harbor::container_rm(container)
    
    if (!file.exists(local_docker_tempfile))
      stop("Sessioninfo was not written to file (it does not exist): ",
           local_docker_tempfile)
    
    message("Wrote sessioninfo from Docker to this tempfile:",
            local_docker_tempfile)
    load(local_docker_tempfile)
    #clean up
    if (deleteTempfiles)
      unlink(local_tempdir, recursive = TRUE)
    return(get("info"))
  }
