# Copyright 2016 Opening Reproducible Research (http://o2r.info)

#pkgs list of packages as returned by sessionInfo
.create_run_install <- function(pkgs, platform, soft, add_self, no_apt) {

  #create RUN expressions
  package_reqs <- character(0)
  cran_packages <-
    github_packages <- local_packages <- other_packages <- pkg_names <- character(0)

  sapply(pkgs,
         function(pkg) {
           #deterine package name
           if ("Package" %in% names(pkg))
             name <- pkg$Package
           else
             stop("Package name cannot be dertermined for ", pkg) #should hopefully never occure
           
           if(name == "containeRit" && !add_self)
             return()

           if ("Priority" %in% names(pkg) &&
               stringr::str_detect(pkg$Priority, "(?i)base")) {
             #packages with these priorities are normally included and don't need to be installed; do nothing
             return()
           }
           #if necessary, determine package dependencies (outside the loop)
           pkg_names <<- append(pkg_names, name)

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
               ". Therefore the package cannot be installed in the docker image.\n"
             )
         })
  

  run_instructions <- list()

  #install system dependencies if necessary
  if(!isTRUE(platform %in% .supported_platforms)){
    warning("The determined platform '", platform, "' is currently not supported for handling system dependencies. Therefore, they  cannot be installed by containerit.")
  }else if(length(pkg_names) > 0){
   # determine package dependencies (if applicable by given platform)
    pkg_dep <- .find_system_dependencies(pkg_names, platform = platform, package_version = pkg$Version, soft = soft)
    #workaround for issue https://github.com/r-hub/sysreqsdb/issues/22 TODO: remove if not needed anymore
    pkg_dep <- unlist(stringr::str_split(pkg_dep, pattern = " "))
    
    package_reqs <- append(package_reqs, pkg_dep)
 
    #remove dublicate system requirements
    package_reqs <- levels(as.factor(package_reqs)) 
    #some packages may not need to be installed, e.g. because they are pre-installed for a certain image
    package_reqs <- package_reqs[!package_reqs %in% no_apt]
    
    # if platform is debian and system dependencies need to be installed
    if(platform ==.debian_platform && length(package_reqs) > 0){
      commands <- "export DEBIAN_FRONTEND=noninteractive; apt-get -y update"
      install_command <- paste("apt-get install -y", paste(package_reqs, collapse = " \\\n\t"))
      commands <- append(commands, install_command)
      run_instructions <- append(run_instructions, Run_shell(commands))
     # run_instructions <-
     #    append(run_instructions,  Run("/bin/sh", params = c("-c","export","DEBIAN_FRONTEND=noninteractive"))) #may use with shell form of Run (to be implemented)
     #  run_instructions <-
     #    append(run_instructions,  Run("apt-get", params = ))
     #  run_instructions <-
     #    append(run_instructions,  Run("apt-get", params = c("update", "-qq", "&&", "install", "-y" , package_reqs)))
    }

    # TODO:'mapping plaftorm > installation' command goes here, analogue to rsysreqs > https://github.com/r-hub/sysreqsdb/tree/master/platforms
  }

  #install cran packages
  if(length(cran_packages) > 0){
    params <- append(paste0("-r '", get_docker_cran_mirror(), "'"), cran_packages)
    run_install_cran <- Run("install2.r", params)
    run_instructions <- append(run_instructions, run_install_cran)
  }

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
    #for more than one package:
    if(length(package)>1){
      out = sapply(package, function(x){
            .find_by_sysreqs_pkg(x, platform, soft, package_version, localFirst) 
      })
      return(out) #there might be dublicate dependencies here but they are removed by the invoking method
    }
      
    sysreqs <- character(0)
    if (localFirst) {
      flog.info("Trying to determine system requirements for package '%s' from the local DESCRIPTION file",
                package)
      path <- find.package(package, quiet = TRUE)
      if (is.null(path) ||
          length(path) == 0 || utils::packageVersion(package) != package_version) {
        flog.warn(
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

    flog.info("Trying to determine system requirements for the package '%s' from the latest DESCRIPTION file on CRAN",
      package)

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
        ", on CRAN. ContaineRit failed to determine system requriements."
      )
      return(NULL)
    } else {
      return(sysreqs)
    }

  }


.find_by_sysreqs_api <-
  function(package, platform) {
    #calls like e.g. https://sysreqs.r-hub.io/pkg/rgdal,curl,rmarkdown/linux-x86_64-ubuntu-gcc are much faster than doing separate calls for each package
    if(length(package)> 0){
      package = paste(package, collapse = ",")
    }
     
    flog.info("Trying to determine system requirements for the package(s) '%s' from sysreq online DB", package)

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
