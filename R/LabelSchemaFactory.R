# Copyright 2017 Opening Reproducible Research (http://o2r.info)

#' Create Build-time labels according to Label Schema Convention 
#'
#' This is a convenience function that generates method for conveniently creating metadata-labels with arguments according to schema version 1.0.0-rc.1
#'  
#' For details about the Label Schema, see http://label-schema.org/rc1/
#'
#' 
#' @return The returned function alows to create labels using the defined label names, e.g. version for org.label-schema.version. 
#' For convenience, dashes are replaced by underscores in argument names. Also, the schema-version is set by default as part of the label.
#' 
#' The names that can be used according to are the following:
#' 
#' schema_version, version, build_date, name, description, usage, url, vcs_url, vcs_ref, vendor, docker.cmd, docker.cmd.devel, 
#' docker.cmd.test, docker.debug, docker.cmd.help, docker.params, rkt.cmd, rkt.cmd.devel, rkt.cmd.test, rkt.debug, rkt.cmd.help, rkt.params
#' 
#' @export
#' @family label
#' 
#' @examples
#' df <- dockerfile(clean_session())
#' factory <- LabelSchemaFactory()
#' label <- factory(name = "ImageName", 
#'   description = "Description of the image",
#'   build_date = Sys.time()
#'  )
#' addInstruction(df) <- label
#' cat(format(df))
#' 
#' 
LabelSchemaFactory <- function(){
  schema_version = "1.0.0-rc.1"
  keys <- readLines(system.file(paste0("label-schema_",schema_version,".txt"), package = "containerit"))
  names <- stringr::str_replace(keys,"^org.label-schema.","")
  names <- stringr::str_replace(names,"-","_")
  keyMap <- keys
  names(keyMap) <- names
  
  
  namesArgs <- rep(NA_character_,length(names))
  names(namesArgs) <- names
  
  namesArgs[["schema_version"]] <- schema_version
  
  ## sligthly modified helper function from
  ## applied, becaus build date needs to follow schema RFC 3339
  ## https://github.com/eddelbuettel/anytime/blob/master/R/formats.R
  rfc3339 <- function(pt) {
    if(is.character(pt))
      return(pt)
    else
      if (inherits(pt, "POSIXt"))
        return(format.POSIXct(pt, "%Y-%m-%dT%H:%M:%OS%z"))
      else if (inherits(pt, "Date"))
        return(format.Date(pt, "%Y-%m-%d"))
      
      warning("Inapplicable object: ", pt)
    invisible(NULL)
  }
  
  factory <- function(){
    label_data <- list()
      sapply(names, function(arg){
      if(is.na(get(arg))){
       # print(arg)
        return()
      } else {
        if(arg == "build_date"){
          value <- rfc3339(get(arg))
          label_data[[keyMap[[arg]]]] <<- value
        }else
          label_data[[keyMap[[arg]]]] <<- get(arg)
        return()
      }
        #message("Argument ", arg, "is set to ", get(arg))
    })
    
    return(new("Label",data = label_data, multi_line = TRUE))
  }
  formals(factory) <- namesArgs
  message <- paste0("According to Label Schema Convention ", schema_version, " you can use the following arguments for constructing metadata labels:")
  #massage <- paste0(message, paste(names, collapse = ", "))
  futile.logger::flog.info(message)
  futile.logger::flog.info(paste(names, collapse = ", "))
  return(factory)
}


# TODO: maybe create a factory with default values?
LabelSchemaFactory2 <- function(){
  factory <- LabelSchemaFactory()
  formals(factory)[["build_date"]] <- Sys.time()
  formals(factory)[["name"]] <- Sys.info()[["user"]] ##actually, this should be the image Name
  
  return(factory)
}

