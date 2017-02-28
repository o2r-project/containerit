# Copyright 2016 Opening Reproducible Research (http://o2r.info)

#' An S4 class to represent a CMD instruction
#' @include Class-Instruction.R
#'
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#cmd}.
#'
#' @slot exec character.
#' @slot params character.
#'
#' @return object of class cmd
#' @export
#'
setClass(
  "Cmd",
  slots = list(exec = "character",
               params = "character"),
  contains = "Instruction"
)

#' create objects representing a CMD instruction
#'
#' @param exec character argument naming the executable
#' @param params paramterer arguments
#'
#' @return An S4 object of class Cmd
#' @export
#'
#' @examples
#' toString(Cmd("R","--vanilla"))
Cmd <- function(exec = NA_character_, params = NA_character_) {
  new("Cmd",  exec = exec, params = params)
}

setValidity(
  "Cmd",
  method = function(object) {
    exec <- slot(object, "exec")
    params <- slot(object, "params")
    
    if (!is.na(exec) &&
        length(exec) == 1 && stringr::str_length(exec) > 0)
      return(TRUE)
    if (is.na(exec) &&
        !any(is.na(params)) &&
        all(stringr::str_length(params) > 0)) {
      return(TRUE)
    }
    if (length(exec) > 1)
      return("More than one exec parameter was given: ", paste(exec, sep = ", "))
    if ((length(params) > 1 && any(is.na(params))) ||
        any(stringr::str_length(params) == 0))
      return("If parameters are given for CMD, they cannot be empty strings or NA")
    
    return(
      "A Cmd instruction must at least have one non-empty exec-argument or one or more parameters given (i.e. as default for an entrypoint)."
    )
  }
)

.arguments.Cmd_Run <- function(obj) {
  # create arcuments in exec form, i.e.
  # ["executable","param1","param2"]
  # or ["param1","param2"] (for CMD as default parameters to ENTRYPOINT)
  
  exec <- slot(obj, "exec")
  params <- slot(obj, "params")
  string <- "["
  if (!is.na(exec)) {
    string <- paste0(string, sprintf('"%s"', exec))
    if (!any(is.na(params)))
      string <- paste0(string, ", ")
  }
  
  if (!any(is.na(params))) {
    paramstr <- sprintf('"%s"', params)
    paramstr <- paste(paramstr, collapse = ", ")
    string <- paste0(string, paramstr)
  }
  string <- paste0(string, "]")
  
  return(string)
}

setMethod("docker_arguments",
          signature(obj = "Cmd"),
          .arguments.Cmd_Run)


#' Create CMD instruction for running an R script
#' 
#' Schema: R [--options] [file] [args]
#'
#' @param path The name of the R script that should run on startup or a path relative to the working directory
#' @param options (optional) Options or flags to be passed to Rscript
#' @param args (otional) Argumands to be passed to the R script
#' @param vanilla Whether R should startup in vanilla mode. Default: TRUE
#' 
#' @return A CMD instruction
#' @export
CMD_Rscript <- function(path, options = character(0), args = character(0), vanilla = TRUE){
  if(vanilla)
    options <- append(options, "--vanilla")
  params <- append(options, c("-f", path))
  params <- append(params, args)
  Cmd("R", params = params)
}



#' Create CMD instruction for rendering a markdown file
#' 
#' Schema: R [--options] -e \"rmarkdown::render(input = [path], output_format = [output_format]\""
#'
#' @param path The name of the R markdown file that should run on startup or a path relative to the working directory
#' @param options (optional) Options or flags to be passed to Rscript
#' @param output_format The output format as in \code{rmakdown::render(...)}
#' @param output_dir The output dir as in \code{rmakdown::render(...)}
#' @param vanilla Whether R should startup in vanilla mode. Default: TRUE
#' 
#' @seealso \link[rmarkdown]{render}
#' 
#' @return A CMD instruction
#' @export
CMD_Render <- function(path, options = character(0), output_format = rmarkdown::html_document(), output_dir = NULL, vanilla = TRUE){
  if(vanilla)
    options <- append(options, "--vanilla")
  params <- options
  render_call <- quote(rmarkdown::render("file", output_format = "format", output_dir = NULL))
  render_call[[2]] <- path
  render_call[[3]] <- substitute(output_format)
  render_call[[4]] <- substitute(output_dir)
  render_call <- deparse(render_call,width.cutoff = 500)
  render_call <- deparse(render_call,width.cutoff = 500) #yes, twice!
  render_call <- stringr::str_replace_all(render_call,"^\\\"|\\\"$","")
  expr <- c("-e", render_call)
  params <- append(params , expr)
  Cmd("R", params = as.character(params))
}

