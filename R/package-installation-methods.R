# Copyright 2017 Opening Reproducible Research (http://o2r.info)

#pkgs list of packages as returned by sessionInfo
.create_run_install <-
  function(.dockerfile,
           pkgs,
           platform,
           soft,
           versioned_libs) {
    #create RUN expressions
    package_reqs <- character(0)
    cran_packages <- character(0)
    github_packages <- character(0)
    local_packages <- character(0)
    other_packages <-  character(0)
    pkg_names <-  character(0)
    package_versions <- character(0)

    sapply(pkgs,
           function(pkg) {
             #determine package name
             if ("Package" %in% names(pkg))
               name <- pkg$Package
             else
               stop("Package name cannot be dertermined for ", pkg) #should hopefully never occure

             if ("Priority" %in% names(pkg) &&
                 stringr::str_detect(pkg$Priority, "(?i)base")) {
               #packages with these priorities are normally included and don't need to be installed; do nothing
               return()
             }
             #if necessary, determine package dependencies (outside the loop)
             pkg_names <<- append(pkg_names, name)
             package_versions <<-
               append(package_versions, pkg$Version)

             #check if package come from CRAN or GitHub
             if ("Repository" %in% names(pkg) &&
                 stringr::str_detect(pkg$Repository, "(?i)CRAN"))
             {
               cran_packages <<- append(cran_packages, pkg$Package)
               return()
             } else if ("RemoteType" %in% names(pkg) &&
                        stringr::str_detect(pkg$RemoteType, "(?i)github"))
             {
               github_packages <<-
                 append(github_packages, getGitHubRef(pkg$Package))
               return()
             }

             else
               warning(
                 "Failed to identify source for package ",
                 pkg$Package,
                 ". Therefore the package cannot be installed in the Docker image.\n"
               )
           })

    image_name <- .dockerfile@image@image

    # installing github packages requires the package 'remotes'
    if (length(github_packages) > 0 &&
        !"remotes" %in% cran_packages &&
        !image_name %in% c("rocker/tidyverse", "rocker/verse", "rocker/geospatial")) {
      cran_packages <- append(cran_packages, "remotes")
      pkg_names <- append(pkg_names, "remotes")
      if (requireNamespace("remotes"))
        package_versions <-
        append(package_versions, utils::packageVersion("remotes"))
      else
        package_versions <- append(package_versions, "latest")
    }

    #install those system dependencies  which are necessary
    if (!isTRUE(platform %in% .supported_platforms)) {
      stop(
        "The determined platform '",
        platform,
        "' is currently not supported for handling system dependencies. Therefore, they  cannot be installed by containerit."
      )
    }

    if (length(pkg_names) > 0) {
      #--- handle dependency exceptions--------------------

      # dependencies that can be left out
      no_apt <- character(0)

      # additional dependencies
      add_apt <- character(0)

      # additional instructions that shall be appended -after- installing system requirements
      add_inst <- list()

      if ("sf" %in% pkg_names) {
        # sf-dependencies proj and gdal cannot be installed directly from apt get, because the available packages are outdated.
        sf_installed <- requireNamespace("sf")

        if (sf_installed &&
            versioned_libs) {
          # Exceptions are handled by json config here:
          ext_soft <- sf::sf_extSoftVersion()
          mapply(function(lib, version) {
            if (!.isVersionSupported(lib, version, .package_config)) {
              msg <-
                paste(
                  "No explicit for support for the version",
                  version,
                  "of the linked external software",
                  lib
                )
              futile.logger::flog.warn(msg)
              return()
            }
            add_apt <<-
              append(add_apt,
                     .get_lib_apt_requirements(lib, version, .package_config))
            no_apt <<-
              append(
                add_apt,
                .get_lib_pkgs_names(
                  lib = lib,
                  platform = .debian_platform,
                  config = .package_config
                )
              )
            add_inst <<-
              append(
                add_inst,
                .get_lib_install_instructions(
                  lib = lib,
                  version = version,
                  config = .package_config
                )
              )
            return(invisible())
          },
          lib = names(ext_soft),
          version = as.character(ext_soft))


          #NOTE: The following is the "old" way to do it. Getting sf to work only requires a more current version gdal, while all other dependencies can be installed from APT
        } else if (!image_name == "rocker/geospatial") {
          futile.logger::flog.info(
            "The dependent package simple features for R requires current versions from gdal, geos and proj that may not be available by standard apt-get.\nWe recommend using the base image rocker/geospatial."
          )
          futile.logger::flog.info("Docker will try to install GDAL 2.1.3 from source")

          add_apt <- append(add_apt, c("wget", "make"))
          add_inst <- append(add_inst, Workdir("/tmp/gdal"))
          add_inst <-
            append(add_inst, Run_shell(
              c(
                "wget http://download.osgeo.org/gdal/2.1.3/gdal-2.1.3.tar.gz",
                "tar zxf gdal-2.1.3.tar.gz",
                "cd gdal-2.1.3",
                "./configure",
                "make",
                "make install",
                "ldconfig",
                "rm -r /tmp/gdal"
              )
            ))
        }
      }

      # we may ad more no-apt or analogue no-package exceptions here and handle them with the json config -
      # as far as we know that certain images have sertain dependencies pre-installed,
      # but at the moment it won't be necessary
      if (image_name == "rocker/geospatial") {
        #these packages are pre-installed
        no_apt <-
          append(no_apt, c("libproj-dev", "libgeos-dev", "gdal-bin"))
      }

      # determine package dependencies (if applicable by given platform)
      pkg_dep <- .find_system_dependencies(
        pkg_names,
        platform = platform,
        package_version = package_versions,
        soft = soft
      )

      # fix duplicates and parsing https://github.com/o2r-project/containerit/issues/79
      pkg_dep_deduped <- unique(unlist(pkg_dep, use.names = FALSE))

      # fix if depends come back with a space https://github.com/r-hub/sysreqsdb/issues/22
      pkg_dep <-
        unlist(lapply(pkg_dep_deduped, function(x) {
          unlist(strsplit(x, split = " "))
        }))

      package_reqs <- append(package_reqs, pkg_dep)

      #some packages may not need to be installed, e.g. because they are pre-installed for a certain image
      package_reqs <- package_reqs[!package_reqs %in% no_apt]
      package_reqs <- append(package_reqs, add_apt)

      #remove dublicate system requirements
      package_reqs <- levels(as.factor(package_reqs))

      # if platform is debian and system dependencies need to be installed
      if (platform == .debian_platform && length(package_reqs) > 0) {
        commands <-
          "export DEBIAN_FRONTEND=noninteractive; apt-get -y update"
        install_command <-
          paste("apt-get install -y",
                paste(package_reqs, collapse = " \\\n\t"))
        commands <- append(commands, install_command)
        addInstruction(.dockerfile)  <- Run_shell(commands)

        if (length(add_inst) > 0)
          addInstruction(.dockerfile) <- add_inst
        # For using the exec form (??):
        #  Run("/bin/sh", params = c("-c","export","DEBIAN_FRONTEND=noninteractive")))
        # Run("apt-get", params = c("update", "-qq", "&&", "install", "-y" , package_reqs)))
      }

    } # length(package_names)


    if (length(cran_packages) > 0) {
      futile.logger::flog.info("Adding CRAN packages: %s", toString(cran_packages))
      params <-
        append(paste0("-r '", get_container_cran_mirror(), "'"),
               cran_packages)
      run_install_cran <- Run("install2.r", params)
      addInstruction(.dockerfile) <- run_install_cran
    }

    if (length(github_packages) > 0) {
      futile.logger::flog.info("Adding GitHub packages: %s", toString(github_packages))
      addInstruction(.dockerfile) <-
        Run("installGithub.r", github_packages)
    }

    return(.dockerfile)
  }

.find_system_dependencies <-
  function(package,
           platform,
           soft,
           method = if (soft == TRUE)
             method = "sysreq-package"
           else
             method = "sysreq-api",
           package_version = utils::packageVersion(package)) {
    # slower, because it analyzes all package DESCRIPTION files of attached / loaded packages.
    # That causes an overhead of database-requests, because dependent packages appear in the sessionInfo as well as in the DESCRIPTION files
    if (method == "sysreq-package")
      return(
        .find_by_sysreqs_pkg(
          package = package,
          package_version = package_version,
          platform = platform,
          soft = soft
        )
      )

    # faster, but only finds direct package dependencies from all attached / loaded packages
    if (method == "sysreq-api")
      return(.find_by_sysreqs_api(package = package, platform = platform))
  }

.find_by_sysreqs_pkg <-
  function(package,
           platform,
           soft,
           package_version,
           localFirst = TRUE) {
    #for more than one package:
    if (length(package) > 1) {
      out = mapply(function(pkg, version) {
        .find_by_sysreqs_pkg(pkg, platform, soft, version, localFirst)
      }, pkg = package, version = package_version)
      return(out) #there might be dublicate dependencies here but they are removed by the invoking method
    }

    sysreqs <- character(0)
    if (localFirst) {
      flog.info(
        "Trying to determine system requirements for package '%s' from the local DESCRIPTION file",
        package
      )
      path <- find.package(package, quiet = TRUE)
      if (is.null(path) ||
          length(path) == 0 ||
          utils::packageVersion(package) != package_version) {
        flog.warn(
          "No matching package DESCRIPTION found locally for package '",
          package,
          "', version '",
          package_version,
          "' ."
        )
      } else{
        sysreqs <-
          sysreqs::sysreqs(file.path(path, "DESCRIPTION"),
                           platform = platform,
                           soft = soft)
        return(sysreqs)
      }
    }

    flog.info(
      "Trying to determine system requirements for the package '%s' from the latest DESCRIPTION file on CRAN",
      package
    )

    con <-
      url(paste0(
        "https://CRAN.R-project.org/package=",
        package,
        "/DESCRIPTION"
      ))
    temp <- tempfile()
    success <- TRUE
    tryCatch({
      desc <- readLines(con)
      writeLines(desc, temp)
      sysreqs <-
        sysreqs::sysreqs(temp, platform = platform, soft = soft)
    }, error = function(e) {
      success <- FALSE
    },
    finally = {
      unlink(temp)
      close(con)
    })

    if (!success) {
      warning(
        "Could not package DESCRIPTION for package '",
        package,
        ", on CRAN. containerit failed to determine system requirements."
      )
      return(NULL)
    } else {
      return(sysreqs)
    }

  }


.find_by_sysreqs_api <-
  function(package, platform) {
    #calls like e.g. https://sysreqs.r-hub.io/pkg/rgdal,curl,rmarkdown/linux-x86_64-ubuntu-gcc are much faster than doing separate calls for each package
    if (length(package) > 0) {
      package = paste(package, collapse = ",")
    }


    package_msg <- stringr::str_replace_all(package, ",", ", ")
    futile.logger::flog.info(
      "Trying to determine system requirements for the package(s) '%s' from sysreq online DB",
      package_msg
    )

    con <-
      url(paste0("https://sysreqs.r-hub.io/pkg/", package, "/", platform))
    success <- TRUE
    desc <- NULL
    tryCatch({
      desc <- readLines(con, warn = FALSE)
      parser <- rjson::newJSONParser()
      parser$addData(desc)
      desc <- as.character(parser$getObject())

    }, error = function(e)
      success <- FALSE,
    finally = {
      close(con)
    })
    if (!success) {
      warning(
        "Containerit failed to determine system requriements for package ",
        package,
        "using sysreq online API"
      )
    }
    return(desc)
  }

#' Get GitHub reference from package
#'
#' If a package is not installed from CRAN, this functions tries to determine if it was installed from GitHub using \code{\link[devtools]{session_info}}.
#'
#' @param pkg The name of the package to retrieve the
#'
#' @return A character string with a short refernce, e.g. \code{r-hub/sysreqs@481d263}
#' @export
#'
#' @examples
#' \dontrun{
#' getGitHubRef(rsysreqs)
#' }
#'
getGitHubRef = function(pkg) {
  if (!requireNamespace(pkg))
    stop("Package ", pkg, " cannot be loaded.")

  si_devtools <- devtools::session_info()
  selected <- si_devtools$packages$package == pkg
  ref_devtools <- si_devtools$packages$source[selected]
  futile.logger::flog.debug("Looking for references for package %s", ref_devtools)

  #try to determine github reference from devools
  if (stringr::str_detect(ref_devtools, "(?i)^GitHub \\(.*/.*@|#.*\\)$")) {
    ref_devtools <-
      stringr::str_replace(ref_devtools, "(?i)^GitHub \\(", replacement = "")
    ref_devtools <-
      stringr::str_replace(ref_devtools, "\\)$", replacement = "")

    futile.logger::flog.debug("GitHub reference for %s found with devtools: %s",
                              pkg,
                              ref_devtools)
    return(ref_devtools)
  } else {
    #alternatively, try with 'normal' sessioninfo (normally does not reference a commit)
    si_regular <- sessionInfo()
    pkgs = c(si_regular$otherPkgs, si_regular$loadedOnly)
    repo <- pkgs[[pkg]]$GithubRepo
    uname <- pkgs[[pkg]]$GithubUsername
    ghr <- pkgs[[pkg]]$GithubRef
    ref = paste0(uname, "/", repo, "@", ghr)

    futile.logger::flog.warn("Exact reference of GitHub package %s could not be determined: %s",
                             pkg,
                             ref)
    return(ref)
  }
}
