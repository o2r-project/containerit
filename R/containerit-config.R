# Copyright 2018 Opening Reproducible Research (https://o2r.info)

# You can use the following code to manually inspect and manipulate the configuration file.
if (FALSE) {
  library("containerit")
  source("R/containerit-config.R")

  current_config <- .containerit_read_config()

  default_config <- .getDefaultConfig()

  rjson::toJSON(default_config)
  jsonlite::toJSON(default_config, pretty = TRUE)

  .get_lib_install_instructions("gdal", "2.1.0", current_config)
  lib <- "GDAL"
  version <- "2.1.0"
  config <- default_config

  .get_lib_config(lib, default_config)
  .get_lib_install_config(lib, version, default_config)
  .isVersionSupported(lib, version, default_config)
  .get_lib_install_instructions(lib, version, default_config)
  .get_lib_apt_requirements(lib, version, default_config)
  .containerit_read_config()

  .get_lib_config(lib, current_config)
  .get_lib_install_config(lib, version, current_config)
  .isVersionSupported(lib, version, current_config)
  .get_lib_install_instructions(lib, version, current_config)
  .get_lib_apt_requirements(lib, version, current_config)

  containerit_write_config(output = "inst/containerit_config.json")
}


#' Re - init (overwrite) current package configuration
#'
#' @param output Path or connection to write the configuration to
#' @param config The configuration object to be written
#'
#' @export
#'
#' @examples
#' containerit_write_config(output = stdout())
#' \dontrun{
#' #developers may want to initialize the default config with:
#' containerit_write_config(output = "inst/containerit_config.json")
#' }
containerit_write_config <-
  function(config = .getDefaultConfig(),
           output = file.path(find.package("containerit"), "inst/containerit_config.json")) {
    # see https://sysreqs.r-hub.io/map/PROJ.4
    config_json <- jsonlite::toJSON(config, pretty = TRUE)
    writeLines(config_json, output)
    futile.logger::flog.info(paste(
      "Config file written to",
      output,
      ". Please reload package or restart session."
    ))
    return(invisible())
  }


.containerit_read_config <-
  function(input = system.file("containerit_config.json",
                               package = "containerit",
                               mustWork = TRUE)) {
    txt <- readLines(input)
    config <- jsonlite::fromJSON(txt)
    return(config)
  }

.addLibSupport <- function(config, value) {
  #use only lower-case names for indices
  name <- stringr::str_to_lower(value$name)
  config$external_libs[[name]] <- value
  return(config)
}

".addLibSupport<-" <- .addLibSupport

.getDefaultConfig <- function() {
  config <- list()
  config$external_libs <- list()
  .addLibSupport(config) <- .createGDALSupport()
  .addLibSupport(config) <- .createPROJSupport()
  return(config)
}

.createLibSupport <-
  function(name,
           apt_pkgs = "",
           installation_config = list()) {
    out <- list(name = name,
               apt_pkgs = apt_pkgs,
               installation_config = installation_config)
    return(out)
  }

.createInstallationConfig <-
  function(supported_versions,
           instructions_template,
           require_apt = "") {
    out <- list(
      supported_versions = supported_versions,
      instructions_template = instructions_template,
      require_apt = require_apt
    )
    return(out)
  }

.createGDALSupport <- function() {
  versions <-
    c(
      "1.10.0",
      "1.10.1",
      "1.11.0",
      "1.11.1",
      "1.11.2",
      "1.11.3",
      "1.11.4",
      "1.11.5",
      "2.0.0",
      "2.0.1",
      "2.0.2",
      "2.0.3",
      "2.1.0",
      "2.1.1",
      "2.1.2",
      "2.1.3"
    )

  # The commands creating the required instructions have to be deparsed to a string,
  # as long as we don't support parsing/deparsing of the instructions themselves
  instructions <- quote(list(Workdir("/tmp/gdal"),
                             Run_shell(
                               c(
                                 "wget http://download.osgeo.org/gdal/[VERSION]/gdal-[VERSION].tar.gz",
                                 "tar zxf gdal-[VERSION].tar.gz",
                                 "cd gdal-[VERSION]",
                                 "./configure",
                                 "make",
                                 "make install",
                                 "ldconfig",
                                 "rm -r /tmp/gdal"
                               )
                             )))
  instructions <- deparse(instructions)

  installation <-
    .createInstallationConfig(versions, instructions, require_apt = c("make", "wget"))

  libSupportObj <-
    .createLibSupport("gdal", "gdal-bin", list(installation))
  return(libSupportObj)
}

.get_lib_config <- function(lib, config) {
  lib <- stringr::str_to_lower(lib)
  config$external_libs[[lib]]
}

.createPROJSupport <- function() {
  versions <-
    c(
      "4.4.0",
      "4.4.1",
      "4.4.2",
      "4.4.3",
      "4.4.4",
      "4.4.5",
      "4.4.6",
      "4.4.7",
      "4.4.8",
      "4.4.9",
      "4.5.0",
      "4.6.0",
      "4.6.1",
      "4.7.0",
      "4.8.0",
      "4.9.0RC1",
      "4.9.0RC2",
      "4.9.0b2",
      "4.9.1",
      "4.9.1RC1",
      "4.9.1RC2",
      "4.9.1RC3",
      "4.9.1RC4",
      "4.9.2",
      "4.9.2",
      "4.9.2RC1",
      "4.9.2RC1",
      "4.9.2RC2",
      "4.9.2RC2",
      "4.9.3",
      "4.9.3",
      "4.9.3RC1",
      "4.9.3RC1",
      "4.9.3RC2",
      "4.9.3RC2",
      "4.9.3RC3",
      "4.9.3RC3"
    )

  # The commands creating the required instructions have to be deparsed to a string,
  # as long as we don't support parsing/deparsing of the instructions themselves
  instructions <- quote(list(Workdir("/tmp/proj"),
                             Run_shell(
                               c(
                                 "wget http://download.osgeo.org/proj/proj-[VERSION].tar.gz",
                                 "tar zxf proj-[VERSION].tar.gz",
                                 "cd proj-[VERSION]",
                                 "./configure",
                                 "make",
                                 "make install",
                                 "ldconfig",
                                 "rm -r /tmp/proj"
                               )
                             )))
  instructions <- deparse(instructions)

  installation <-
    .createInstallationConfig(versions, instructions, require_apt = c("make", "wget"))

  libSupportObj <-
    .createLibSupport("proj.4", "libproj-dev", list(installation))
  return(libSupportObj)
}

.get_lib_config <- function(lib, config) {
  lib <- stringr::str_to_lower(lib)
  config$external_libs[[lib]]
}

#get platform dependend package names (i.e. apt packages), as far as known
.get_lib_pkgs_names <-
  function(lib, platform, config = .package_config) {
    if (!(platform %in% c(.debian_platform, .ubuntu_platform)))
      #may use also sysreqs db for this query, e.g. https://sysreqs.r-hub.io/map/GDAL
      return(NULL)
    return(unlist(.get_lib_config(lib, config)[["apt_pkgs"]]))
  }

.get_lib_install_config <- function(lib, version, config) {
  libconfig <- .get_lib_config(lib, config)
  output <- NULL
  if (!is.null(libconfig)) {
    config_list <- libconfig$installation_config
    # a profane way to check whether the list of installations
    # has been simplifyed to one single item:
    if ("supported_versions" %in% names(config_list)) {
      config_list <- list()
      config_list[[1]] <- libconfig$installation_config
    }
    sapply(config_list,
           function(installation) {
             if (version %in% unlist(installation$supported_versions))
               output <<- installation
             }
           , simplify = TRUE)
  }
  return(output)
}

#test if versioned installation for a lib is configured
.isVersionSupported <- function(lib, version, config) {
  return(!is.null(.get_lib_install_config(lib, version, config)))
}

.get_lib_install_instructions <- function(lib, version, config) {
  install_config <- .get_lib_install_config(lib, version, config)
  if (is.null(install_config))
    return(NULL)
  else{
    template <- unlist(install_config$instructions_template)
    instructions_string <-
      stringr::str_replace_all(template, pattern = "\\[VERSION\\]", replacement = version)
    instructions <- eval(parse(text = instructions_string))
    return(instructions)
  }
}

.get_lib_apt_requirements <- function(lib, version, config) {
  install_config <- .get_lib_install_config(lib, version, config)
  if (is.null(install_config))
    return(NULL)
  else{
    return(unlist(install_config[["require_apt"]]))
  }
}
