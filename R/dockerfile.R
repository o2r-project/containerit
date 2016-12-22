# Copyright 2016 Opening Reproducible Research (http://o2r.info)

#' dockerfile
#'
#' Create a Dockerfile from the
#'
#' @param from the source of the information to construct the Dockerfile
#' @param to path and file name to save the Dockerfile to
#' @param env the environment that should be included in the image
#' @param maintainer optionally specify the maintainer of the dockerfile (see Maintainter-class)
#' @param r_version optionally specify the R version that should run inside the container (uses the current version that runs on host, by default)
#'
#' @return An object of class Dockerfile
#' @export
#'
#' @examples
#' dockerfile()
#'
#' @import futile.logger
dockerfile <- function(from = utils::sessionInfo(), to = paste0(getwd(), "/", "Dockerfile"), env = NULL, maintainer = NULL, r_version = paste(R.Version()$major, R.Version()$minor, sep=".")) {
  flog.debug("Creating a new Dockerfile from %s to %s", from, to)
  .dockerfile <- NA
  .originalFrom <- class(from)
  
  #Instructions that create a basic dockerfile (from could be NULL)
  
  path = to
  image=paste("rocker/r-ver",r_version, sep=":")  #TODO: figure out if R-version is available from Rocker
  instructions=list()
  instructions = append(instructions, paste("FROM", image))
  
  
  if(!is.null(maintainer)){
    cmd =  paste("MAINTAINER", paste0("\"", slot(maintainer, "name"),"\""), slot(maintainer, "email"))  #Default maintainer?!
    instructions = append(instructions, cmd)
  }else
    warning("No dockerfile maintainer was specified!")
  
  cmd = paste("CMD [\"R\"]")  #May not be necessary in future
  instructions = append(instructions, cmd)
  
  .dockerfile = new("Dockerfile", instructions=instructions, maintainer=maintainer, image=image, path=path, context=NA_character_)  #TODO: context really NA?
  
  
  if(inherits(x = from, "sessionInfo")) {
    .dockerfile <- dockerfileFromSession(session = from, to = to, .dockerfile = .dockerfile)
  } else if (inherits(x = from, "file")) {
    .dockerfile <- dockerfileFromFile(file = from, to = to, .dockerfile = .dockerfile)
  } else if(inherits(x = from, "character") && dir.exists(from)) {
    .dockerfile <- dockerfileFromWorkspace(path = from, to, .dockerfile = .dockerfile)
    .originalFrom <- from
  }else if(is.null(from)) {
    message("A simple dockerfile will be created that only specifies the given maintainer and R version.") 
  }else {
    stop("Unsupported 'from': ", class(from), from)
  }

  
  flog.info("Created Dockerfile at %s based on %s", to, .originalFrom)
  message("Created Dockerfile at", to, " based on ", .originalFrom, ". Use 'write'-method for serialization.")
  return(.dockerfile)
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
#' write.Dockerfile(dockerfile(to=temp)) 
#' print(readLines(temp)) 
#' unlink(temp)
write.Dockerfile = function(x, file = slot(x, "path")){
  return(write(as.character(slot(x,"instructions")), file))
}
  
 

dockerfileFromSession <- function(session, to, .dockerfile) {

  
  

  return(.dockerfile)
}

dockerfileFromFile <- function(file, to, .dockerfile) {
  return(.dockerfile)
}

dockerfileFromWorkspace <- function(path, to, .dockerfile) {
  .rFiles <- dir(path = path, pattern = "\\.R$", full.names = TRUE, include.dirs = FALSE, recursive = TRUE)

  return(.dockerfile)
}
