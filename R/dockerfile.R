# Copyright 2018 Opening Reproducible Research (https://o2r.info)

#' dockerfile-method
#'
#' Create a Dockerfile based on either a sessionInfo, a workspace or a file.
#'
#' @section Based on \code{sessionInfo}:
#'
#' Use the current \code{\link[utils]{sessionInfo})} to create a Dockerfile.
#'
#' @section Based on a workspace/directory:
#'
#' Given an existing path to a directory, the method tries to automatically find the main \code{R} file within that directory.
#' Files are searched recursively. The following types are supported:
#'
#' \enumerate{
#'   \item regular R script files, identified by file names ending in \code{.R}
#'   \item weaved documents, identified by file names ending in \href{http://rmarkdown.rstudio.com/}{R Markdown} (\code{.Rmd})
#' }
#'
#' After identifying the main file, the process continues as described in the section file.
#' If both types are found, documents are given priority over scripts.
#' If multiple files are found, the first file as returned by \code{\link[base]{dir}} will be used.
#'
#' @section Based on a file:
#'
#' Given an executable \code{R} script or document, create a Dockerfile to execute this file.
#' This executes the whole file to obtain a complete \code{sessionInfo} object, see section "Based on \code{sessionInfo}", and copies required files and documents into the container.
#'
#' @param from The source of the information to construct the Dockerfile. Can be a \code{sessionInfo} object, a path to a file, or the path to a workspace). If \code{NULL} then no automatic derivation of dependencies happens.
#' @param image (\linkS4class{From}-object or character) Specifes the image that shall be used for the Docker container (\code{FROM} instruction).
#'      By default, the image is determinded from the given session. Alternatively, use \code{getImageForVersion(..)} to get an existing image for a manually defined version of R, matching the version with tags from the base image rocker/r-ver (see details about the rocker/r-ver at \url{'https://hub.docker.com/r/rocker/r-ver/'}). Or provide a correct image name yourself.
#' @param maintainer Specify the maintainer of the dockerfile. See documentation at \url{'https://docs.docker.com/engine/reference/builder/#maintainer'}. Defaults to \code{Sys.info()[["user"]]}. Can be removed with \code{NULL}.
#' @param save_image When TRUE, it calls \link[base]{save.image} in the current working directory and copys the resulting \code{.RData} file to the container's working directory. The created file in the local working director will not be deleted.
#'  Alternatively, you can pass a list of objects to be saved, which may also include arguments to be passed down to \code{save}, e.g. \code{save_image = list("object1", "object2")}. You can configure the name of the file the objects are saved to by adding a file name to the list of arguments, e.g. \code{save_image = list("objectA", save_image_filename = "mydata.RData")}, in which case the file path must be in UNIX notation. Note that you may not use \code{save_image_filename} for other objects in your session!
#' \code{save} will be called with \code{envir}.
#' @param envir The environment for \code{save_image}.
#' @param env optionally specify environment variables to be included in the image. See documentation: \url{'https://docs.docker.com/engine/reference/builder/#env}
#' @param soft (boolean) Whether to include soft dependencies when system dependencies are installed, default is no.
#' @param offline (boolean) Whether to use an online database to detect system dependencies or use local package information (slower!), default is no.
#' @param copy whether and how a workspace should be copied - values: "script", "script_dir" or a list of relative file paths to be copied, or \code{NA} ot disable copying of files
#' @param container_workdir the working directory in the container, defaults to \code{/payload/} and must end with \code{/}. Can be skipped with value \code{NULL}.
#' @param cmd The CMD statement that should be executed by default when running a parameter. Use cmd_Rscript(path) in order to reference an R script to be executed on startup
#' @param entrypoint the ENTRYPOINT statement for the Dockerfile
#' @param add_self Whether to add the package containerit itself if loaded/attached to the session
#' @param silent Whether or not to print information during execution
#' @param predetect Extract the required libraries based on \code{library} calls using the package \code{automagic} before running a script/document
#' @param versioned_libs [EXPERIMENTAL] Whether it shall be attempted to match versions of linked external libraries
#' @param versioned_packages [EXPERIMENTAL] Whether it shall be attempted to match versions of R packages
#' @param filter_baseimage_pkgs Do not add packages from CRAN that are already installed in the base image. This does not apply to non-CRAN dependencies, e.g. packages install from GitHub.
#'
#' @return An object of class Dockerfile
#'
#' @export
#'
#' @import futile.logger
#' @importFrom utils capture.output
#' @importFrom stringr str_detect regex str_extract str_length str_sub
#'
#' @examples
#' dockerfile <- dockerfile()
#' print(dockerfile)
#'
dockerfile <- function(from = utils::sessionInfo(),
                       image = getImageForVersion(getRVersionTag(from)),
                       maintainer = Sys.info()[["user"]],
                       save_image = FALSE,
                       envir = .GlobalEnv,
                       env = list(generator = paste("containerit", utils::packageVersion("containerit"))),
                       soft = FALSE,
                       offline = FALSE,
                       copy = NA, # "script",
                       # nolint start
                       container_workdir = "/payload/",
                       # nolint end
                       cmd = Cmd("R"),
                       entrypoint = NULL,
                       add_self = FALSE,
                       silent = FALSE,
                       predetect = TRUE,
                       versioned_libs = FALSE,
                       versioned_packages = FALSE,
                       filter_baseimage_pkgs = FALSE) {
    if (silent) {
      invisible(futile.logger::flog.threshold(futile.logger::WARN))
    }

    the_dockerfile <- NA
    originalFrom <- class(from)

    #parse From-object from string if necessary
    if (is.character(image)) {
      image <- parseFrom(image)
    }
    futile.logger::flog.debug("Creating a new Dockerfile from '%s' with base image %s", from, toString(image))

    if (is.character(maintainer)) {
      .label <- Label_Maintainer(maintainer)
      futile.logger::flog.debug("Turning maintainer character string '%s' into label: %s", maintainer, toString(.label))
      maintainer <- .label
    }

    # check CMD instruction
    if (!inherits(x = cmd, "Cmd")) {
      stop("Unsupported parameter for 'cmd', expected an object of class 'Cmd', given was :",
        class(cmd))
    }

    # check ENTRYPOINT instruction
    if ( !is.null(entrypoint) && !inherits(x = entrypoint, "Entrypoint")) {
      stop("Unsupported parameter for 'entrypoint', expected an object of class 'Entrypoint', given was :",
        class(entrypoint))
    }

    # check and create WORKDIR instruction
    workdir <- NULL
    if ( !is.null(container_workdir)) {
      if ( !is.character(container_workdir)) {
        stop("Unsupported parameter for 'container_workdir', expected a character string or NULL")
      } else {
        # nolint start
        if (!stringr::str_detect(container_workdir, "/$")) {
          # nolint end
          # directories given as destination must have a trailing slash in dockerfiles
          container_workdir <- paste0(container_workdir, "/")
          futile.logger::flog.info("Appended trailing slash, workdir is '%s'", container_workdir)
        }
        workdir <- Workdir(container_workdir)
      }
    }

    # check whether image is supported
    image_name <- image@image
    if (!image_name %in% .supported_images) {
      warning("Unsupported base image. Proceed at your own risk. The following base images are supported:\n",
        paste(.supported_images, collapse = "\n"))
    }

    # base dockerfile
    the_dockerfile <- methods::new("Dockerfile",
                                instructions = list(),
                                maintainer = maintainer,
                                image = image,
                                entrypoint = entrypoint,
                                cmd = cmd)

    # handle different "from" cases
    if (is.null(from)) {
      futile.logger::flog.debug("from is NULL, not deriving any information at all")
      if (!is.null(workdir))
        addInstruction(the_dockerfile) <- workdir
    } else if (inherits(from, "expression")
               || (is.list(from) && all(sapply(from, is.expression)))
               ) {
      futile.logger::flog.debug("Creating from expression object with a clean session %s", toString(from))
      the_session <- clean_session(expr = from,
                               predetect = predetect,
                               echo = !silent)
      the_dockerfile <- dockerfileFromSession(session = the_session,
                                              dockerfile = the_dockerfile,
                                              soft = soft,
                                              offline = offline,
                                              add_self = add_self,
                                              versioned_libs = versioned_libs,
                                              versioned_packages = versioned_packages,
                                              filter_baseimage_pkgs = filter_baseimage_pkgs,
                                              workdir = workdir)
    } else if (inherits(x = from, "sessionInfo")) {
      futile.logger::flog.debug("Creating from sessionInfo object")
      the_dockerfile <- dockerfileFromSession(session = from,
                                           dockerfile = the_dockerfile,
                                           soft = soft,
                                           offline = offline,
                                           add_self = add_self,
                                           versioned_libs = versioned_libs,
                                           versioned_packages = versioned_packages,
                                           filter_baseimage_pkgs = filter_baseimage_pkgs,
                                           workdir = workdir)
    } else if (inherits(x = from, "character")) {
      futile.logger::flog.debug("Creating from character string '%s'", from)

      if (dir.exists(from)) {
        futile.logger::flog.debug("'%s' is a directory", from)
        originalFrom <- from
        the_dockerfile <- dockerfileFromWorkspace(path = from,
                                               dockerfile = the_dockerfile,
                                               soft = soft,
                                               offline = offline,
                                               add_self = add_self,
                                               copy = copy,
                                               silent = silent,
                                               predetect = predetect,
                                               versioned_libs = versioned_libs,
                                               versioned_packages = versioned_packages,
                                               filter_baseimage_pkgs = filter_baseimage_pkgs,
                                               workdir = workdir)
      } else if (file.exists(from)) {
        futile.logger::flog.debug("'%s' is a file", from)
        originalFrom <- from
        the_dockerfile <- dockerfileFromFile(file = from,
                                          dockerfile = the_dockerfile,
                                          soft = soft,
                                          offline = offline,
                                          add_self = add_self,
                                          copy = copy,
                                          silent = silent,
                                          predetect = predetect,
                                          versioned_libs = versioned_libs,
                                          versioned_packages = versioned_packages,
                                          filter_baseimage_pkgs = filter_baseimage_pkgs,
                                          workdir = workdir)
      } else {
        stop("Unsupported string for 'from' argument (not a file, not a directory): ", from)
      }
    } else {
      stop("Unsupported 'from': ", class(from), " ", from)
    }

    # copy additional objects into the container in an RData file
    .filename = ".RData"
    if ("save_image_filename" %in% names(save_image)) {
      .filename <- save_image$save_image_filename
    }
    if (isTRUE(save_image)) {
      futile.logger::flog.debug("Saving image to file %s with %s and adding COPY instruction using environment %s",
                                .filename, toString(ls(envir = envir)),
                                utils::capture.output(envir))
      save(list = ls(envir = envir), file = .filename, envir = envir)
      addInstruction(the_dockerfile) <- Copy(src = .filename, dest = .filename)
    } else if (is.list(save_image)) {
      futile.logger::flog.debug("Saving image using to file %s and adding COPY instruction based on %s",
                                .filename, toString(save_image))
      save(list = unlist(save_image[names(save_image) != "save_image_filename"]), file = .filename, envir = envir)
      addInstruction(the_dockerfile) <- Copy(src = .filename, dest = .filename)
    }

    futile.logger::flog.info("Created Dockerfile-Object based on %s", originalFrom)
    return(the_dockerfile)
}

dockerfileFromPackages <- function(pkgs,
                                   dockerfile,
                                   soft,
                                   offline,
                                   versioned_libs,
                                   versioned_packages,
                                   filter_baseimage_pkgs,
                                   workdir) {
  futile.logger::flog.debug("Creating from packages data.frame")

  # The platform is determined only for known images.
  # Alternatively, we could let the user optionally specify one amongst different supported platforms
  platform = NULL
  image_name = dockerfile@image@image
  if (image_name %in% .debian_images) {
    platform = .debian_platform
    futile.logger::flog.debug("Found image %s in list of Debian images", image_name)
  }
  futile.logger::flog.debug("Detected platform: %s", platform)

  the_dockerfile <- add_install_instructions(dockerfile = dockerfile,
                                     pkgs = pkgs,
                                     platform = platform,
                                     soft = soft,
                                     offline = offline,
                                     versioned_libs = versioned_libs,
                                     versioned_packages = versioned_packages,
                                     filter_baseimage_pkgs = filter_baseimage_pkgs)

  # after all installation is done, set the workdir
  addInstruction(the_dockerfile) <- workdir

  return(the_dockerfile)
}

dockerfileFromSession <- function(session, ...) {
  UseMethod("dockerfileFromSession", session)
}

dockerfileFromSession.sessionInfo <- function(session,
                                              dockerfile,
                                              soft,
                                              offline,
                                              add_self,
                                              versioned_libs,
                                              versioned_packages,
                                              filter_baseimage_pkgs,
                                              workdir) {
  futile.logger::flog.debug("Creating from sessionInfo")

  apks <- session$otherPkgs
  lpks <- session$loadedOnly
  pkgs <- append(apks, lpks) # packages to be installed

  if (!add_self && !is.null(pkgs$containerit)) {
    futile.logger::flog.debug("Removing self from the list of packages")
    pkgs$containerit <- NULL
  }

  # 1. identify where to install the package from
  pkgs_list <- lapply(pkgs, function(pkg) {
           #determine package name
           if ("Package" %in% names(pkg))
             name <- pkg$Package
           else
             stop("Package name cannot be determined for ", pkg)

           if ("Priority" %in% names(pkg) &&
               stringr::str_detect(pkg$Priority, "(?i)base")) {
             futile.logger::flog.debug("Skipping Priority package %s, is included with R", name)
             return(NULL)
           } else {
             version <- NA
             source <- NA

             #check if package come from CRAN, GitHub or Bioconductor
             if ("Repository" %in% names(pkg) &&
                 stringr::str_detect(pkg$Repository, "(?i)CRAN")) {
               source <- "CRAN"
               version <- pkg$Version
             } else if ("RemoteType" %in% names(pkg) &&
                        stringr::str_detect(pkg$RemoteType, "(?i)github")) {
               source <- "github"
               version <- getGitHubRef(name, pkgs)
             } else if ("biocViews" %in% names(pkg)) {
               source <- "Bioconductor"
               version <- pkg$Version
             }
               else {
               warning("Failed to identify a source for package ", name,
                       ". Therefore the package cannot be installed in the Docker image.\n")
             }

             return(list(name = name, version = version, source = source))
           }
         })

  # remove NULLs
  pkgs_list <- pkgs_list[!vapply(pkgs_list, is.null, logical(1))]

  packages_df <- do.call("rbind", lapply(pkgs_list, as.data.frame))
  futile.logger::flog.debug("Found %s packages in sessionInfo", nrow(packages_df))

  the_dockerfile <- dockerfileFromPackages(pkgs = packages_df,
                                        dockerfile = dockerfile,
                                        soft = soft,
                                        offline = offline,
                                        versioned_libs = versioned_libs,
                                        versioned_packages = versioned_packages,
                                        filter_baseimage_pkgs = filter_baseimage_pkgs,
                                        workdir = workdir)
  return(the_dockerfile)
}

dockerfileFromSession.session_info <- function(session,
                                              dockerfile,
                                              soft,
                                              offline,
                                              add_self,
                                              versioned_libs,
                                              versioned_packages,
                                              filter_baseimage_pkgs,
                                              workdir) {
  futile.logger::flog.debug("Creating from session_info")

  if (is.null(session$packages) || !(inherits(session$packages, "packages_info")))
    stop("Unsupported object of class session_info, needs list slot 'packages' of class 'packages_info'")

  if ("loadedversion" %in% names(session$packages)) {
    # sessioninfo
    packages_df <- session$packages[,c("package", "loadedversion", "source")]
  } else {
    # devtools
    packages_df <- session$packages[,c("package", "version", "source")]
  }
  names(packages_df) <- c("name", "version", "source")

  if (!add_self) {
    futile.logger::flog.debug("Removing self from the list of packages")
    packages_df <- packages_df[packages_df$name != "containerit",]
  }

  # create version strings as we want them for GitHub packages
  pkgs_gh <- packages_df[stringr::str_detect(string = packages_df$source, stringr::regex("GitHub", ignore_case = TRUE)),]
  if (nrow(pkgs_gh) > 0) {
    for (pkg in pkgs_gh$name) {
      currentPkg <- subset(pkgs_gh, pkgs_gh$name == pkg)
      versionString <- stringr::str_extract(string = currentPkg$source, pattern = "(?<=\\()(.*)(?=\\))")
      packages_df[packages_df$name == pkg,c("version")] <- versionString
    }
  }

  the_dockerfile <- dockerfileFromPackages(pkgs = packages_df,
                                        dockerfile = dockerfile,
                                        soft = soft,
                                        offline = offline,
                                        versioned_libs = versioned_libs,
                                        versioned_packages = versioned_packages,
                                        filter_baseimage_pkgs = filter_baseimage_pkgs,
                                        workdir = workdir)
  return(the_dockerfile)
}

dockerfileFromFile <- function(file,
                               dockerfile,
                               soft,
                               copy,
                               offline,
                               add_self,
                               silent,
                               predetect,
                               versioned_libs,
                               versioned_packages,
                               filter_baseimage_pkgs,
                               workdir) {
    futile.logger::flog.debug("Creating from file")

    # prepare context ( = working directory) and normalize paths:
    context = normalizePath(getwd())
    file = normalizePath(file)
    futile.logger::flog.debug("Working with file %s in working directory %s", file, context)

    #Is the file within the context?
    len = stringr::str_length(context)
    substr = stringr::str_sub(context, end = len)
    if (context != substr)
      stop("The given file is not inside the context directory!")

    # make sure that the path is relative to context
    rel_path <- .makeRelative(file, context)

    # execute script / markdowns or read RData file to obtain sessioninfo
    if (stringr::str_detect(string = file,
                            pattern = stringr::regex(".R$", ignore_case = TRUE))) {
      futile.logger::flog.info("Processing R script file '%s' locally.", rel_path)
      sessionInfo <- clean_session(script_file = file,
                                   echo = !silent,
                                   predetect = predetect)
    } else if (stringr::str_detect(string = file,
                                   pattern = stringr::regex(".rmd$", ignore_case = TRUE))) {
      futile.logger::flog.info("Processing Rmd file '%s' locally using rmarkdown::render(...)", rel_path)
      sessionInfo <- clean_session(rmd_file = file,
                                   echo = !silent,
                                   predetect = predetect)
    } else if (stringr::str_detect(string = file,
                                   pattern = stringr::regex(".rdata$", ignore_case = TRUE))) {
      futile.logger::flog.info("Extracting session object from RData file %s", rel_path)
      sessionInfo <- extract_session_file(file)
    } else{
      futile.logger::flog.info("The supplied file %s has no known extension. containerit will handle it as an R script for packaging.", rel_path)
    }

    # append system dependencies and package installation instructions
    the_dockerfile <- dockerfileFromSession(session = sessionInfo,
                                         dockerfile = dockerfile,
                                         soft = soft,
                                         offline = offline,
                                         add_self = add_self,
                                         versioned_libs = versioned_libs,
                                         versioned_packages = versioned_packages,
                                         filter_baseimage_pkgs = filter_baseimage_pkgs,
                                         workdir = workdir)

    ## working directory must be set before. Now add copy instructions
    if (!is.null(copy) && !is.na(copy)) {
      copy = unlist(copy)
      if (!is.character(copy)) {
        stop("Invalid argument given for 'copy'")
      } else if (length(copy) == 1 && copy == "script") {
        #unless we use some kind of Windows-based Docker images, the destination path has to be unix compatible:
        rel_path_dest <- stringr::str_replace_all(rel_path, pattern = "\\\\", replacement = "/")
        addInstruction(the_dockerfile) <- Copy(rel_path, rel_path_dest)
      } else if (length(copy) == 1 && copy == "script_dir") {
        script_dir <- normalizePath(dirname(file))
        rel_dir <- .makeRelative(script_dir, context)

        #unless we use some kind of Windows-based Docker images, the destination path has to be unix compatible:
        rel_dir_dest <- stringr::str_replace_all(rel_dir, pattern = "\\\\", replacement = "/")

        # directories given as destination must have a trailing slash in dockerfiles
        if (!stringr::str_detect(rel_dir_dest, "/$"))
          rel_dir_dest <- paste0(rel_dir_dest, "/")

        # let's also add a trailing slash to the source dir
        if (!stringr::str_detect(rel_dir, "/$"))
          rel_dir <- paste0(rel_dir, "/")

        addInstruction(the_dockerfile) <- Copy(rel_dir, rel_dir_dest)
      } else {
        futile.logger::flog.debug("Seems we have a list of paths/files in 'copy': ", toString(file))
        sapply(copy, function(file) {
          if (file.exists(file)) {
            futile.logger::flog.debug("Adding copy command for file ", file)
            rel_path <- .makeRelative(normalizePath(file), context)
            rel_path_dest <- stringr::str_replace_all(rel_path, pattern = "\\\\", replacement = "/")
            if (dir.exists(file) && !stringr::str_detect(rel_path_dest, "/$"))
              rel_path_dest <- paste0(rel_dir_dest, "/")

            addInstruction(the_dockerfile) <<- Copy(rel_path, rel_path_dest)
          } else {
            warning("The file ", file, ", given by 'copy', does not exist! Invalid argument.")
          }
        })
      }
    }

    return(the_dockerfile)
  }

dockerfileFromWorkspace <- function(path,
                                   dockerfile,
                                   soft,
                                   offline,
                                   add_self,
                                   copy,
                                   silent,
                                   predetect,
                                   versioned_libs,
                                   versioned_packages,
                                   filter_baseimage_pkgs,
                                   workdir) {
    futile.logger::flog.debug("Creating from workspace directory")
    target_file <- NULL #file to be packaged

    .rFiles <- dir(path = path,
                   pattern = "\\.R$",
                   full.names = TRUE,
                   include.dirs = FALSE,
                   recursive = TRUE,
                   ignore.case = TRUE)

    .md_Files <- dir(path = path,
                     pattern = "\\.Rmd$", #|\\.Rnw$",
                     full.names = TRUE,
                     include.dirs = FALSE,
                     recursive = TRUE,
                     ignore.case = TRUE)
    futile.logger::flog.debug("Found %s scripts and %s documents", length(.rFiles), length(.md_Files))

    if (length(.rFiles) > 0 && length(.md_Files) > 0) {
      target_file <- .md_Files[1]
      warning("Found both scripts and weaved documents (Rmd) in the given directory. Using the first document for packaging: \n\t",
              target_file)
    } else if (length(.md_Files) > 0) {
      target_file <- .md_Files[1]
      if (length(.md_Files) > 1)
        warning("Found ", length(.md_Files), " document files in the workspace, using '", target_file, "'")
    } else if (length(.rFiles) > 0) {
      target_file <- .rFiles[1]
      if (length(.rFiles) > 1)
        warning("Found ", length(.rFiles), " script files in the workspace, using '", target_file, "'")
    }

    if (is.null(target_file))
      stop("Workspace does not contain any R file that can be packaged.")
    else
      futile.logger::flog.info("Found file for packaging in workspace: %s", target_file)

    the_dockerfile <- dockerfileFromFile(target_file,
                              dockerfile = dockerfile,
                              soft = soft,
                              offline = offline,
                              copy = copy,
                              add_self = add_self,
                              silent = silent,
                              predetect = predetect,
                              versioned_libs = versioned_libs,
                              versioned_packages = versioned_packages,
                              filter_baseimage_pkgs = filter_baseimage_pkgs,
                              workdir = workdir)
    return(the_dockerfile)
  }


#' getImageForVersion-method
#'
#' Get a suitable Rocker image based on the R version.
#' Needs network access to retrieve the available images.
#'
#' If there was no matching image found, a warning is issued.
#'
#' @param r_version A string representation of the R version, e.g. "3.4.2"
#' @param nearest A boolean, should the closest version be returned if there is no match?
#'
#' @return A string with the name of the Docker image
#' @export
#' @examples
#' getImageForVersion(getRVersionTag(utils::sessionInfo()))
#' getImageForVersion("3.4.3")
#'
#' @importFrom semver parse_version
getImageForVersion <- function(r_version, nearest = TRUE) {
  #check if dockerized R version is available (maybe check other repositories too?)
  tags <- .tagsfromRemoteImage(.rocker_images[["versioned"]])
  image <- From(.rocker_images[["versioned"]], tag = r_version)

  closestMatch <- function(version, versions) {
    if (version %in% versions) return(version);

    factors <- list(major = 1000000, minor = 1000, patch = 1)

    semver <- semver::parse_version(version)[[1]]
    semver_num <- semver$major * factors[["major"]] +
      semver$minor * factors[["minor"]] +
      semver$patch * factors[["patch"]]

    sorted_semvers <- sort(semver::parse_version(versions))

    offsets <- sapply(X = sorted_semvers, FUN = function(v) {
      v_num <- v$major * factors[["major"]] +
        v$minor * factors[["minor"]] +
        v$patch * factors[["patch"]]
      return(abs(semver_num - v_num))
    })

    min_offset = min(offsets)
    return(sorted_semvers[which(offsets == min_offset)])
  }

  if (!r_version %in% tags) {
    if (nearest) {
      # get numeric versions with all parts (maj.min.minor), i.e. two dots
      numeric_tags <- tags[which(grepl("\\d.\\d.\\d", tags))]
      closest <- as.character(closestMatch(r_version, numeric_tags))
      image <- From(.rocker_images[["versioned"]], tag = closest)

      warning("No Docker image found for the given R version, returning closest match: ",
              closest,
              " Existing tags (list only available when online): ",
              paste(tags, collapse = " ")
      )
    } else {
      warning("No Docker image found for the given R version, returning input. ",
              "Existing tags (list only available when online): ",
              paste(tags, collapse = " ")
      )
    }
  }

  return(image)
}

.tagsfromRemoteImage <- function(image) {
  urlstr <- paste0("https://registry.hub.docker.com/v2/repositories/",
                   image,
                   "/tags/?page_size=9999")
  str <- NULL

  futile.logger::flog.debug("Retrieving tags for image %s with %s", image, urlstr)
  tryCatch({
    con <- url(urlstr)
    str <- readLines(con, warn = FALSE)
    },
    error = function(e) {
      stop("Could not retrieve existing tags from ", urlstr, " (offline?), error: ", e)
    },
    finally = close(con))

  if (is.null(str)) {
    return(c())
  } else {
    parser <- rjson::newJSONParser()
    parser$addData(str)
    tags <- sapply(parser$getObject()$results, function(x) {
      x$name
    })
    return(tags)
  }
}

.makeRelative <- function(files, from) {
  out <- sapply(files, function(file) {
    len = stringr::str_length(from)
    rel_path = stringr::str_sub(file, start = len + 1)
    if (stringr::str_detect(rel_path, "^[\\/]"))
      rel_path = stringr::str_sub(rel_path, start = 2)
    if (stringr::str_length(rel_path) == 0)
      rel_path <- "."
    return(rel_path)
  })
  as.character(out)
}
