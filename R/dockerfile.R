# Copyright 2016 Opening Reproducible Research (http://o2r.info)

#' dockerfile
#'
#' Create a Dockerfile from the
#'
#' @param from the source of the information to construct the Dockerfile
#' @param env the environment that should be included in the image
#' @param maintainer optionally specify the maintainer of the dockerfile (see Maintainter-class)
#' @param r_version (character) optionally specify the R version that should run inside the container (default: the current R version that runs on host)
#'    containeRit will try to find a corresponding docker image as a base (if not sepcified otherwise)
#' @param image (From-object or character) optionally specify the image that shall be used for the docker container (FROM-statement)
#' @param context (character vector) optionally specify one or many build context paths
#'
#' @return An object of class Dockerfile
#' @export
#'
#' @examples
#' dockerfile()
#'
#' @import futile.logger
dockerfile <- function(from = utils::sessionInfo(),env = NULL, maintainer = NULL, r_version = paste(R.Version()$major, R.Version()$minor, sep="."), image = NULL, context = NA_character_) {
  flog.debug("Creating a new Dockerfile from %s", from)
  .dockerfile <- NA
  .originalFrom <- class(from)
  
  #Instructions that create a basic dockerfile (from could be NULL)
  
  if(is.null(image)) {
    #check if dockerized R version is available (maybe check other repositories too?)
    tags <- tagsfromRemoteImage("rocker/r-ver")
      if(r_version %in% tags){
        image=From("rocker/r-ver",tag = r_version)
      }else{
        stop("No docker image found for the given R version. ", 
            "Please either specify a custom Docker image or \n", 
             "  use one of the following supported version tags (maybe check the internet connection). \n\t", 
             paste(tags, collapse = " "))
      }
  } 
  #parse From-object from string if necessary
  if (is.character(image)) {
    image = parseFrom(image)
  }
  
  instructions=list()
  
  cmd = paste("CMD [\"R\"]")  #May not be necessary in future
  instructions = append(instructions, cmd)
  
  .dockerfile = new("Dockerfile", instructions=instructions, maintainer=maintainer, image=image, context=context)
  
  
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

#' Format a dockerfile object to a series of instruction
#'
#' @param x An object of class Dockerfile
#'  @param ... Arguments to be passed down to format.default
#'
#' @return The Content of the dockerfile represented by the dockerfile object, by default formated as a List of strings where each string represent a new line
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
  return(format(output, ...))
}


#' Write a dockerfile
#'
#' @param x Dockerfile-object to be serialized
#' @param file optional argument for specifying a costum file path 
#'
#' @export
#'
#' @examples
#' # write a dockerfile with default parameters to temporary file and show content:
#' temp = tempfile()
#' write.Dockerfile(dockerfile(), file=temp) 
#' print(readLines(temp)) 
#' unlink(temp)
write.Dockerfile = function(x, file = paste0(getwd(), "/", "Dockerfile")){
  flog.info("Writing dockerfile to %s", file)
  message("Writing dockerfile to ", file)
  return(write(as.character(format(x)), file))
}
  
 

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
