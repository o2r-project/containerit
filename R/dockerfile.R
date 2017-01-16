# Copyright 2016 Opening Reproducible Research (http://o2r.info)

#' dockerfile-method
#'
#' Create a Dockerfile based on either a sessionInfo, a workspace or a file
#'
#' @param from (sessionInfo, file or a string specifying the path to a workspace) The source of the information to construct the Dockerfile
#' @param objects character vector naming all R objects to be included in the docker image / R session. Can be character(0) (default), ls() or a fraction of ls()
#' @param maintainer optionally specify the maintainer of the dockerfile. See the \code{Maintainter-class} and the official documentation: \url{'https://docs.docker.com/engine/reference/builder/#maintainer'}
#' @param r_version (character) optionally specify the R version that should run inside the container. By default, the current R version is used.
#' @param image (From-object or character) optionally specify the image that shall be used for the docker container (FROM-statement)
#'      By default, the image is determinded from the given r_version, while the version is matched with tags from the base image rocker/r-ver
#'      see details about the rocker/r-ver at \url{'https://hub.docker.com/r/rocker/r-ver/'}
#' @param env optionally specify environment variables to be included in the image. See documentation: \url{'https://docs.docker.com/engine/reference/builder/#env}
#' @param context (character vector) optionally specify one or many build context paths
#'
#' @return An object of class Dockerfile
#' @export
#'
#' @examples
#' dockerfile()
#'
#' @import futile.logger
dockerfile <- function(from = utils::sessionInfo(), objects=character(0), maintainer = Maintainer(name = Sys.info()[["user"]]), r_version = paste(R.Version()$major, R.Version()$minor, sep="."), image = imagefromRVersion(r_version), env = list(generator = paste("containerit", packageVersion("containerit"))), context = NA_character_) {
  flog.debug("Creating a new Dockerfile from %s", from)
  .dockerfile <- NA
  .originalFrom <- class(from)
  
  #parse From-object from string if necessary
  if (is.character(image)) {
    image = parseFrom(image)
  }
  
  instructions=list()
  cmd = Cmd("R") ### default CMD may be overwritten e.g. from dockerfileFromSession
  .dockerfile = new("Dockerfile", instructions=instructions, maintainer=maintainer, image=image, context=context, cmd=cmd)
  
  
  if(inherits(x = from, "sessionInfo")) {
    .dockerfile <- dockerfileFromSession(session = from, .dockerfile = .dockerfile)
  } else if (inherits(x = from, "file")) {
    .dockerfile <- dockerfileFromFile(file = from, .dockerfile = .dockerfile)
  } else if(inherits(x = from, "character") && dir.exists(from)) {
    .dockerfile <- dockerfileFromWorkspace(path = from, .dockerfile = .dockerfile)
    .originalFrom <- from
  }else if(is.null(from)) {
      #Creates a basic dockerfile without the 'from'-argument 
  }else {
    stop("Unsupported 'from': ", class(from), from)
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
#' @return The content of the dockerfile represented by the Dockerfile object, by default formatted as a list of strings where each string represent a new line
#' @export
#'
#' @examples
#' format(dockerfile())
format.Dockerfile <- function(x, ...){
  #initialize dockerfile with from
  output = list()
  from = toString(slot(x,"image"))
  output = append(output, from)
  maintainer = slot(x,"maintainer")
  if(!is.null(maintainer))
    output = append(output, toString(maintainer))
  instructions = slot(x,"instructions")
  if(!is.null(instructions) && length(instructions) > 0){
    instructions = sapply(instructions, toString)
    output = append(output, unlist(instructions))
  }
  cmd = slot(x,"cmd")
  if(!is.null(cmd))
    output = append(output, toString(cmd))
  return(format(output, ...))
}



.write.Dockerfile = function(x, file = paste0(getwd(), "/", "Dockerfile")){
  flog.info("Writing dockerfile to %s", file)
  message("Writing dockerfile to ", file)
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
setMethod("write", signature(x = "Dockerfile", file = "character"), .write.Dockerfile)
  
 

dockerfileFromSession <- function(session, .dockerfile) {
  return(.dockerfile)
}

dockerfileFromFile <- function(file, .dockerfile) {
  return(.dockerfile)
}

dockerfileFromWorkspace <- function(path, .dockerfile) {
  .rFiles <- dir(path = path, pattern = "\\.R$", full.names = TRUE, include.dirs = FALSE, recursive = TRUE)

  return(.dockerfile)
}



imagefromRVersion = function(r_version){
  #check if dockerized R version is available (maybe check other repositories too?)
  tags <- tagsfromRemoteImage("rocker/r-ver")
  if(!r_version %in% tags){
    warning("No Docker image found for the given R version. ", 
         "You might want to specify a custom Docker image or \n", 
         "  use one of the following supported version tags (maybe check the internet connection if no suggestions appear). \n\t", 
         paste(tags, collapse = " "))
  }
  
  image=From("rocker/r-ver",tag = r_version)
  return(image)
}

tagsfromRemoteImage = function(image){
  urlstr = paste0("https://registry.hub.docker.com/v2/repositories/", image,"/tags/")
  con=url(urlstr)
  str=readLines(con, warn=FALSE);str
  close(con)
  parser <- rjson::newJSONParser()
  parser$addData(str)
  tags=sapply(parser$getObject()$results, function(x){x$name})
  return(tags)
}
