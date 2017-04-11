#!/usr/bin/Rscript

#See related project issue at https://github.com/o2r-project/containerit/issues/12

"container_it.R is a command-line interface to the R package containerit. 
It packages R sessions, scripts, workspaces and vignettes together with all dependencies to execute them in Docker containers.

Usage:  container_it.R dir [options] [--copy arg] [-d <DIR>]
        container_it.R file [options] [--copy arg] [--cmd-render <FORMAT> | --cmd-R-file] <FILE>
        container_it.R session [options] [-e <EXPR> ...]
        container_it.R [--help | --version]

Modes:
    dir       Searches the given directory for R / Rmarkdown files and uses the first encounter for packaging
    file      Packages a given R script or R markdown file. 
              Optionally, the container can run a batch execution or rendering of the given file instead of an interactive R session. 
    session   Packages an empty R session in which a series of R commands may be executed (see option -e)
  
Options (for all modes):
  --force -f          Force writing output even if a file of the same name already exists
  --image <ARG>       Specify the Docker image that shall be used for the docker container (FROM-statement)
                      By default, the image is determinded from the given r_version, 
                      while the version is matched with tags from the base image rocker/r-ver
                      see details about the rocker/r-ver at https://hub.docker.com/r/rocker/r-ver/'
  --maintainer -m <ARG>   Name / email of the dockerfile's maintainer (will be read from environment variables if not given)
  --no-write          Don't write dockerfile to output file
  --no-vanilla        Package a session / file without using the vanilla flag 
                      (warning: site and environment files currently cannot be included in the container)
  --output -o FILE    Path and name of the output Dockerfile [default: ./Dockerfile]
  --print -p          Print dockerfile to the console
  --r_version -r <ARG>  Specify an R version number that should run inside the container. 
                      By default, the version of the currently linked R instance is used.
  --save -s <objects> ...    Save a list of objects from the workspace to an .RData file (overwrites --save-image)
  --save-image -i     Save the current workspace to an .RData file
  --soft              Whether to include soft dependencies among the system dependencies of R packages. [default: FALSE]
  --quiet -q          Run containerit as silent as possible (print only errors and warnings) [default: FALSE]

Other:
  --copy -c script | script_dir | <copy_file> ...     
                      Indicates whether and how a workspace should be copied. 
                      For the modes 'dir' and 'file' containerit copies either the given input file, the complete directory 
                      or a list of individual files and directories that should be located below the current directory. 
                      [default: script]
  -d <DIR>            Directory to be used for packaging in mode 'dir'. Use either current dir or a folder below. [default: ./]
  -e <EXPR>           Expression do be executed within the R session that shall be packaged ('session' mode).
  --cmd-render all | pdf | html   
                      Indicates that the given R markdown file should be rendered when running the container
  --cmd-R-file        Indicates that the give R script should be executed on container startup (uses R -f example.R)
  --help -h           print usage and exit
  --version -v        print version and exit

Examples:
  # runs the first r-script / r-script file locally and prints the corresponding dockerfile
  container_it.R dir -p --no-write  
  
  # Runs the given R-script locally and saves a workspace image.
  # The Dockerfile will be written to the current directory and overwritten if it already exists
  # It contains instructions to copy the script and workspace image and to execute the script on start-up
  container_it.R file -ifp --cmd-R-file path/example.R

  # Creates an empty R session and executes the given R commands, which later can be reproduced in the docker container
  # The container will opperate with the given R version 3.3.0
  container_it.R session -p -e \"library(sp)\" -e \"demo(meuse, ask=FALSE)\" --r_version 3.3.0

" -> doc
  
library(methods)
library(containerit)

if(length(commandArgs(trailingOnly=TRUE))== 0){
  # the line below creates an error within R sessions (bug?)
  # opts <- docopt::docopt(doc, "--help")
  cat(doc)
} else {
  opts <- docopt::docopt(doc , version = paste0("containerit ", utils::packageVersion("containerit"),"\n"))
  futile.logger::flog.debug(opts)
 

  if(opts[["quiet"]])
    invisible(futile.logger::flog.threshold(futile.logger::WARN))
  
  output_file =  opts[["output"]]
  if(file.exists(output_file) && !opts[["force"]] && !opts[["--no-write"]])
    stop("Dockerfile cannot be written because the target file already exists.\n",
         "\t Re-run with option -f / --force in order to overwride the file or use the flag --no-write.")

    
  if (is.null(opts[["maintainer"]])){
    #use default argument from dockerfile-method
    this_maintainer <- eval(formals(dockerfile)$maintainer)
  }else
    this_maintainer <- opts[["maintainer"]]
  
  
  save_image <- opts[["save-image"]]
  #save image may be overwritten by save:
  if (!is.null(opts[["save"]])){
    save_image <- opts[["save"]]
  }
  
  #mode-specific handling:
  if (opts$session == TRUE){
    expr <- list()
    if (!is.null(opts[["-e"]])){
      expr <- parse(text = unlist(opts[["-e"]]))
    }
    invisible(futile.logger::flog.info("Packaging a new R session..."))
    from <- clean_session(expr = expr,vanilla = !opts[["no-vanilla"]], slave = opts[["quiet"]])
    this_cmd <- eval(formals(dockerfile)$cmd)
  }else{
    
    if (opts$dir == TRUE){
      from <- unlist(opts[["d"]])
    }else if (opts$file == TRUE){
      from <- unlist(opts[["<FILE>"]])
    }else{
      stop("Invalid arguments!")
    }

    if (!is.null(opts[["cmd-render"]]) &&
       opts$file == TRUE) {
      if (opts[["cmd-render"]] == "all")
        this_cmd <- CMD_Render(from, output_format = rmarkdown::all_output_formats())
      else if (opts[["cmd-render"]] == "pdf")
        this_cmd <- CMD_Render(from, output_format = rmarkdown::pdf_document())
      else  if (opts[["cmd-render"]] == "html")
        this_cmd <- CMD_Render(from, output_format = rmarkdown::html_document())

    } else if (opts[["cmd-R-file"]] && opts$file == TRUE) {
      this_cmd <- CMD_Rscript(from)
    } else
      this_cmd <- eval(formals(dockerfile)$cmd
      )
  }

  dockerfile_args <- list(from = from,
                          copy = opts$copy,
                          silent = opts$quiet,
                          save_image = save_image,
                          maintainer = this_maintainer,
                          cmd = this_cmd,
                          soft = opts$soft)
  
  #some arguments should be left to method-default if not explicitely given:
  if (!is.null(opts[["image"]]))
    dockerfile_args$image <- opts$image
  
  if (!is.null(opts[["r_version"]]))
    dockerfile_args$r_version <- opts$r_version

  df_obj <- do.call("dockerfile", dockerfile_args)

   if (!opts[["no-write"]]){
      write(df_obj, file = output_file)
  }
  
  if (opts[["print"]]){
    futile.logger::flog.info("Dockerfile printout: \n")
    cat(format(df_obj), sep = "\n")
  }

}