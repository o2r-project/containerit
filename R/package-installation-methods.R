# Copyright 2016 Opening Reproducible Research (http://o2r.info)

#pkgs list of packages as returned by sessionInfo
.create_run_install <- function(pkgs, platform, soft) {

  #create RUN expressions
  package_reqs <- character(0)
  cran_packages <-
    github_packages <- local_packages <- other_packages <- character(0)

  sapply(pkgs,
         function(pkg) {
           #deterine package name
           if ("Package" %in% names(pkg))
             name <- pkg$Package
           else
             stop("Package name cannot be dertermined for ", pkg) #should hopefully never occure
           
           if ("Priority" %in% names(pkg) &&
               stringr::str_detect(pkg$Priority, "(?i)base")) {
             #packages with these priorities are normally included and don't need to be installed; do nothing
             return()
           }
           #determine package dependencies (if applicable by given platform)
           if(isTRUE(platform %in% .supported_platforms)){
             pkg_dep <- .find_system_dependencies(name, platform = platform, package_version = pkg$Version, soft = soft)
             package_reqs <<- append(package_reqs, pkg_dep)
           }
           
           #check if package come from CRAN (alternatively you may use devtools::session_info)
           if ("Repository" %in% names(pkg) &&
               stringr::str_detect(pkg$Repository, "(?i)CRAN"))
           {
             cran_packages <<- append(cran_packages, pkg$Package)
             return()
             ##TODO: handle github and other package sources
           } else
             warning(
               "Failed to identify source for package ",
               pkg$Package,
               ". Therefore the package cannot be installed in the docker image."
             )
         })
  
  run_instructions <- list()
  package_reqs <-
    levels(as.factor(package_reqs)) #remove dublicate system requirements
  
  #install system dependencies
  if(!isTRUE(platform %in% .supported_platforms)){
    warning("The determined platform '", platform, "' is currently not supported for handling system dependencies. Therefore, they  cannot be installed by containerit.")
  }else if (length(package_reqs) > 0) {
    # if platform contains debian
    if(platform ==.debian_platform){
      run_instructions <-
        append(run_instructions,  Run("/bin/sh", params = c("-c","export","DEBIAN_FRONTEND=noninteractive"))) #may use with shell form of Run (to be implemented)
      run_instructions <-
        append(run_instructions,  Run("apt-get", params = c("update", "-qq")))
      run_instructions <-
        append(run_instructions,  Run("apt-get", params = c("install", "-y" , package_reqs)))
    }

    # TODO:'mapping plaftorm > installation' command goes here, analogue to rsysreqs > https://github.com/r-hub/sysreqsdb/tree/master/platforms
  }
  
  #install cran packages
  params <- append(paste0("-r '", get_docker_cran_mirror(), "'"), cran_packages)
  run_install_cran <- Run("install2.r", params)
  run_instructions <- append(run_instructions, run_install_cran)
  
  # TODO: install packages from other sources
  return(run_instructions)
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
  function(package, platform, soft, package_version, localFirst = TRUE) {
    sysreqs <- character(0)
    if (localFirst) {
      message(
        "Trying to determine system requirements for package '",
        package,
        "' from the local DESCRIPTION file"
      )
      path <- find.package(package, quiet = TRUE)
      if (is.null(path) ||
          length(path) == 0 || utils::packageVersion(package) != package_version) {
        message(
          "No matching package DESCRIPTION found locally for package '",
          package,
          "', version '",
          package_version,
          "' ."
        )
      } else{
        
        sysreqs <- sysreqs::sysreqs(file.path(path, "DESCRIPTION"), platform = platform, soft = soft)
        return(sysreqs)
      }
    }
    
    message(
      "Trying to determine system requirements for package '",
      package,
      "' from the latest DESCRIPTION file on CRAN"
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
      sysreqs <- sysreqs::sysreqs(temp, platform = platform, soft = soft)
    }, error = function(e)
      success <- FALSE,
    finally = {
      unlink(temp)
      close(con)
    })
    if (!success) {
      warning(
        "Could not package DESCRIPTION for package '",
        package,
        ", on CRAN. Containerit failed to determine system requriements."
      )
    }
    
    return(sysreqs)
  }


.find_by_sysreqs_api <-
  function(package, platform) {
    message(
      "Trying to determine system requirements for package '",
      package,
      "' from sysreq online DB"
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
