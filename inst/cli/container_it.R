#!/usr/bin/Rscript

"Usage: container_it.R dir [options] [-d DIR]
        container_it.R file [options] <FILE>
        container_it.R session [options] [-e <exp> ...]
        container_it.R -h | --help

Modes:
      dir       Searches the given directory for R / Rmarkdown files and uses the first encounter for packaging
      file      Packages a given R markdown
      session   rqrrq

Options:
  -o FILE       specify output file [default: ./Dockerfile]
  --copy -c script | script_dir | <copy_file> ...    indicates whether and how a workspace should be copied [default: script]
  --force -f    force writing output even if a file of the same name already exists
  --print -p    print dockerfile by end of execution
  --save-image [<objects> ...] save image or optionally a list of objects from the workspace to an .RData file
  --silent -s   run containeRit as silent as possible (print only errors and warnings) [default: FALSE]
  --no-write    don't write dockerfile to output file
  --no-vanilla  package vanilla session

Other:
  -d DIR      directory which containeRit shall use for packaging [default: ./]
  -e <expr>
  -h --help   print usage and exit
" -> doc
  
library(methods)
library(containeRit)
opts <- docopt::docopt(doc)

opts
#sessionInfo()


if(opts[["silent"]])
  invisible(futile.logger::flog.threshold(futile.logger::WARN))

output_file =  opts[["-o"]]
if(file.exists(output_file) && !opts[["force"]])
  stop("Dockerfile cannot be written because the target file already exists. Re-run with option -f / --force in order to overwride the file.")

if(opts$session == TRUE){
  expr <- list()
  if(opts[["-e"]]){
    expr <- parse(text = unlist(opts[["<exp>"]]))
  }
  invisible(futile.logger::flog.info("Packaging a new R session..."))
  session <- clean_session(expr = expr,vanilla = !opts[["no-vanilla"]], slave = opts[["silent"]])
  df_obj <- dockerfile(session)
}else{ 
  if(opts$dir == TRUE){
    from <- unlist(opts[["d"]])
  }else if(opts$file == TRUE){
    from <- unlist(opts[["<FILE>"]])
  }else{
    stop("Invalid arguments!")  
  }
  df_obj <- dockerfile(from, copy = opts[["copy"]])
}

if("df_obj" %in% ls()){
  if(!opts[["no-write"]]){
      write(df_obj, file = output_file)
  }
}