# Copyright 2017 Opening Reproducible Research (http://o2r.info)

# pkgs is a list of packages as returned by sessionInfo()
# function returns a the dockerfile with the required instructions
.create_run_install <-
  function(dockerfile,
           pkgs,
           platform,
           soft,
           offline,
           versioned_libs,
           versioned_packages,
           filter_baseimage_pkgs,
           filter_deps_by_image = FALSE) {
    package_reqs <- character(0)
    cran_packages <- character(0)
    github_packages <- character(0)
    pkg_names <-  character(0)
    package_versions <- character(0)

    # 0. Packages can be left out because they are pre-installed for given image
    image_name <- dockerfile@image@image
    if (filter_baseimage_pkgs && !versioned_packages) {
      image <- docker_arguments(dockerfile@image)
      available_pkgs <- get_installed_packages(image = image)$pkg
      skipable <- names(pkgs) %in% available_pkgs
      skipped_str <- toString(sort(names(pkgs[skipable])))
      futile.logger::flog.info("Skipping packages for image %s (packages are unversioned): %s",
                               image, skipped_str)
      addInstruction(dockerfile) <- Comment(text = paste0("Packages skipped because they are in the base image: ",
                                                          skipped_str))
      pkgs <- pkgs[!skipable]
    }

    # 1. identify where to install the package from - FIXME do not split into seperate lists but put in a data.frame
    sapply(pkgs,
           function(pkg) {
             #determine package name
             if ("Package" %in% names(pkg))
               name <- pkg$Package
             else
               stop("Package name cannot be dertermined for ", pkg)

             if ("Priority" %in% names(pkg) &&
                 stringr::str_detect(pkg$Priority, "(?i)base")) {
               futile.logger::flog.debug("Skipping Priority package %s, is included with R", name)
             } else {
               #if necessary, determine package dependencies (outside the loop)
               pkg_names <<- append(pkg_names, name)
               package_versions <<- append(package_versions, pkg$Version)

               #check if package come from CRAN or GitHub
               if ("Repository" %in% names(pkg) &&
                   stringr::str_detect(pkg$Repository, "(?i)CRAN")) {
                 cran_packages <<- append(cran_packages, pkg$Package)
               } else if ("RemoteType" %in% names(pkg) &&
                          stringr::str_detect(pkg$RemoteType, "(?i)github")) {
                 github_packages <<- append(github_packages, getGitHubRef(pkg$Package, pkgs))
               } else
                 warning("Failed to identify a source for package ",
                         pkg$Package,
                         ". Therefore the package cannot be installed in the Docker image.\n")
             }
           })

    # Installing github packages requires the package 'remotes'
    if (length(github_packages) > 0 &&
        !"remotes" %in% cran_packages &&
        !image_name %in% c("rocker/tidyverse", "rocker/verse", "rocker/geospatial")) {
      cran_packages <- append(cran_packages, "remotes")
      pkg_names <- append(pkg_names, "remotes")
      #if (requireNamespace("remotes"))
      #  package_versions <-
      #  append(package_versions, utils::packageVersion("remotes"))
      #else
      package_versions <- append(package_versions, "latest")
    }

    if (is.null(platform)) {
      warning("Platform could not be detected, proceed at own risk.")
    } else if (!isTRUE(platform %in% .supported_platforms)) {
      warning("The determined platform '",
              platform,
              "' is currently not supported for handling system dependencies. Therefore, the created manifests might not work.")
    }

    # 2. get system dependencies if packages must be installed
    if (length(pkg_names) > 0) {
      # see package-installation-bespoke.R for some outdated code
      # add_inst <- list()

      #  determine package dependencies (if applicable by given platform)
      package_reqs <- .find_system_dependencies(
        pkg_names,
        platform = platform,
        soft = soft,
        offline = offline,
        package_version = package_versions
      )

      # system dependencies that can be left out because they are pre-installed for given image
      if (filter_deps_by_image) {
        skipable <- .skipable_deps(image_name)
        package_reqs <- package_reqs[!package_reqs %in% skipable]
        futile.logger::flog.info("Skipping deps for image %s: %s", image_name, toString(skipable))
      }

      # remove dublicate system requirements and sort (to increase own reproducibility)
      package_reqs <- levels(as.factor(package_reqs))
      package_reqs <- sort(package_reqs)

      # if platform is debian and system dependencies need to be installed
      if (length(package_reqs) > 0) {
        if (platform == .debian_platform) {
          commands <- "export DEBIAN_FRONTEND=noninteractive; apt-get -y update"
          install_command <- paste("apt-get install -y",
                                   paste(package_reqs, collapse = " \\\n\t"))
          commands <- append(commands, install_command)
          addInstruction(dockerfile)  <- Run_shell(commands)

          #if (length(add_inst) > 0)
          #  addInstruction(dockerfile) <- add_inst
          # For using the exec form (??):
          #  Run("/bin/sh", params = c("-c", "export", "DEBIAN_FRONTEND=noninteractive")))
          #  Run("apt-get", params = c("update", "-qq", "&&", "install", "-y" , package_reqs)))
        } else {
          warning("Platform ", platform, " not supported, cannot add installation commands for system requirements.")
        }
      }
    } # length(package_names)

    if (length(cran_packages) > 0) {
      cran_packages <- sort(cran_packages) # sort, to increase own reproducibility
      futile.logger::flog.info("Adding CRAN packages: %s", toString(cran_packages))
      addInstruction(dockerfile) <- Run("install2.r", cran_packages)
    }

    if (length(github_packages) > 0) {
      github_packages <- sort(github_packages) # sort, to increase own reproducibility
      futile.logger::flog.info("Adding GitHub packages: %s", toString(github_packages))
      addInstruction(dockerfile) <- Run("installGithub.r", github_packages)
    }

    return(dockerfile)
  }

.find_system_dependencies <- function(package,
                                      platform,
                                      soft = TRUE,
                                      offline = FALSE,
                                      package_version = utils::packageVersion(package)) {
  method = if (offline == TRUE)
    method = "sysreq-package"
  else
    method = "sysreq-api"

  .dependencies <- NA

  futile.logger::flog.info("Going online? %s  ... to retrieve system dependencies (%s)", !offline, method)
  futile.logger::flog.debug("Retrieving sysreqs for %s packages and platform %s: %s", length(package), platform, toString(package))

    # slower, because it analyzes all package DESCRIPTION files of attached / loaded packages.
    # That causes an overhead of database-requests, because dependent packages appear in the sessionInfo as well as in the DESCRIPTION files

  if (method == "sysreq-package") {
    .dependencies <- .find_by_sysreqs_pkg(
        package = package,
        package_version = package_version,
        platform = platform,
        soft = soft
      )
  }

  # faster, but only finds direct package dependencies from all attached / loaded packages
  if (method == "sysreq-api") {
    .dependencies <- .find_by_sysreqs_api(package = package, platform = platform)

    if (length(.dependencies) > 0) {
      # remove duplicates and unlist dependency string from sysreqs
      .dependencies <- unique(unlist(.dependencies, use.names = FALSE))
      .dependencies <- unlist(lapply(.dependencies, function(x) {
        unlist(strsplit(x, split = " "))
      }))
    }
  }

  futile.logger::flog.debug("Found %s system dependencies: %s", length(.dependencies), toString(.dependencies))
  return(.dependencies)
}

.find_by_sysreqs_pkg <- function(package,
                                 platform,
                                 soft = TRUE,
                                 package_version,
                                 localFirst = TRUE) {
    # for more than one package:
    if (length(package) > 1) {
      out <- mapply(function(pkg, version) {
        .find_by_sysreqs_pkg(pkg, platform, soft, version, localFirst)
      }, pkg = package, version = package_version)
      return(out) # there might be dublicate dependencies, they must be removed by the invoking method
    }

    sysreqs <- character(0)
    if (localFirst) {
      futile.logger::flog.info(
        "Trying to determine system requirements for package '%s' from the local DESCRIPTION file",
        package
      )
      path <- find.package(package, quiet = TRUE)
      if (is.null(path) ||
          length(path) == 0 ||
          utils::packageVersion(package) != package_version) {
        futile.logger::flog.warn(
          "No matching package DESCRIPTION found locally for package '",
          package,
          "', version '",
          package_version,
          "' ."
        )
      } else{
        sysreqs <- NA
        if (is.null(platform)) {
          futile.logger::flog.warn("Platform could not be determined, possibly because of unknown base image.",
                                   " Using '%s'", sysreqs::current_platform())
          sysreqs <-
            sysreqs::sysreqs(file.path(path, "DESCRIPTION"),
                             soft = soft)
        } else {
          sysreqs <-
            sysreqs::sysreqs(file.path(path, "DESCRIPTION"),
                             platform = platform,
                             soft = soft)
        }
        return(sysreqs)
      }
    }

    futile.logger::flog.info("Trying to determine system requirements for '%s' from the DESCRIPTION file on CRAN",
      package)

    con <- url(paste0("https://CRAN.R-project.org/package=",
                      package,
                      "/DESCRIPTION"))
    temp <- tempfile()
    success <- TRUE
    tryCatch({
      desc <- readLines(con)
      writeLines(desc, temp)
      sysreqs <-
        sysreqs::sysreqs(temp, platform = platform, soft = soft)
    }, error = function(e) {
      success <- FALSE
      futile.logger::flog.debug("Error requesting system requirements from DESCRIPTION file on CRAN: %s", toString(e))
    },
    finally = {
      unlink(temp)
      close(con)
    })

    if (!success) {
      warning("Could not package DESCRIPTION for package '",
        package,
        ", on CRAN. containerit failed to determine system requirements.")
      return(NULL)
    } else {
      return(sysreqs)
    }

  }


.find_by_sysreqs_api <- function(package, platform) {
    # calls like e.g. https://sysreqs.r-hub.io/pkg/rgdal,curl,rmarkdown/linux-x86_64-ubuntu-gcc are much faster than doing separate calls for each package
    if (length(package) > 0) {
      package = paste(package, collapse = ",")
    }

    futile.logger::flog.info("Trying to determine system requirements for the package(s) '%s' from sysreqs online DB", package)

    .url <- paste0("https://sysreqs.r-hub.io/pkg/", package, "/", platform)
    con <- url(.url)
    futile.logger::flog.debug("Accessing '%s'", .url)
    success <- TRUE
    desc <- NULL

    tryCatch({
      desc <- readLines(con, warn = FALSE)
      futile.logger::flog.debug("Response: %s", toString(desc))
      parser <- rjson::newJSONParser()
      parser$addData(desc)
      desc <- as.character(parser$getObject())
      desc <- desc[!desc == "NULL"]
    }, error = function(e) {
      success <- FALSE
      futile.logger::flog.debug("Error requesting package info from sysreqs online DB: %s", toString(e))
    }, finally = close(con))

    if (!success) {
      warning("Containerit failed to determine system requriements for package ",
        package, "using sysreq online API")
    }

    futile.logger::flog.debug("Dependencies info from sysreqs online DB:\n%s", toString(desc))
    return(desc)
  }

#' Get GitHub reference from package
#'
#' If a package is installed from GitHub this function tries to retrieve the reference (i.e. repository accoutn, name, and commit) from
#'
#' \enumerate{
#'   \item the provided sessionInfo
#'   \item locally, and only if the package is installed (!), using \code{\link[devtools]{session_info}}
#' }
#'
#' @param pkg The name of the package to retrieve the reference for
#' @param pkgs Lists of packages from a sessionInfo object
#'
#' @return A character string with a short refernce, e.g. \code{r-hub/sysreqs@481d263}, \code{NA} is nothign could be found
#' @export
#'
#' @examples
#' \dontrun{
#' getGitHubRef(rsysreqs)
#' }
#'
getGitHubRef = function(pkg, pkgs = c(sessionInfo()$otherPkgs, sessionInfo()$loadedOnly)) {
  ref <- NA_character_

  if (!is.null(pkgs[[pkg]]$GithubRepo))
    repo <- pkgs[[pkg]]$GithubRepo
  else repo <- pkgs[[pkg]]$RemoteRepo

  if (!is.null(pkgs[[pkg]]$GithubUsername))
    uname <- pkgs[[pkg]]$GithubUsername
  else uname <- pkgs[[pkg]]$RemoteUsername

  if (!is.null(pkgs[[pkg]]$GithubSHA1))
    ghr <- pkgs[[pkg]]$GithubSHA1
  else ghr <- pkgs[[pkg]]$RemoteSha

  if (any(sapply(X = c(repo, uname, ghr), FUN = is.null))) {
    futile.logger::flog.warn("Exact reference of GitHub package %s could not be determined from session info: %s %s %s",
                             pkg, repo, uname, ghr)
  } else {
    ref = paste0(uname, "/", repo, "@", ghr)
  }

  if (is.na(ref)) {
    if (requireNamespace(pkg))
    #try to determine github reference from devools
    si_devtools <- devtools::session_info()
    ref_devtools <- si_devtools$packages$source[si_devtools$packages$package == pkg]
    futile.logger::flog.debug("Looking for references with devtools for package %s", ref_devtools)

    if (stringr::str_detect(ref_devtools, "(?i)^GitHub \\(.*/.*@|#.*\\)$")) {
      ref_devtools <- stringr::str_replace(ref_devtools, "(?i)^GitHub \\(", replacement = "")
      ref_devtools <- stringr::str_replace(ref_devtools, "\\)$", replacement = "")
      futile.logger::flog.debug("GitHub reference for %s found with devtools: %s",
                                pkg,
                                ref_devtools)
      ref <- ref_devtools
    } else
      futile.logger::flog.warn("GitHub ref is unknown, but package %s is not available locally, no fallback.", pkg)
  }
  return(ref)
}

