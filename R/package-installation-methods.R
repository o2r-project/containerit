# Copyright 2017 Opening Reproducible Research (http://o2r.info)

#pkgs list of packages as returned by sessionInfo
.create_run_install <- function(.dockerfile, pkgs, platform, soft) {

  #create RUN expressions
  package_reqs <- character(0)
  cran_packages <-  github_packages <- local_packages <- other_packages <- pkg_names <- package_versions <- character(0)

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
           #if necessary, determine package dependencies (outside the loop)
           pkg_names <<- append(pkg_names, name)
           package_versions <<- append(package_versions, pkg$Version)

           #check if package come from CRAN (alternatively you may use devtools::session_info)
           if ("Repository" %in% names(pkg) &&
               stringr::str_detect(pkg$Repository, "(?i)CRAN"))
           {
             cran_packages <<- append(cran_packages, pkg$Package)
             return()
           } else if ("RemoteType" %in% names(pkg) &&
                            stringr::str_detect(pkg$RemoteType, "(?i)github"))
           {
             github_packages <<- append(github_packages, .getGitHubRef(pkg$Package))
             return()
           }
           
           else
             warning(
               "Failed to identify source for package ",
               pkg$Package,
               ". Therefore the package cannot be installed in the docker image.\n"
             )
         })
  
  image_name <- .dockerfile@image@image

  #installing github packages requires devtools
  if(length(github_packages) > 0 && !"devtools" %in% cran_packages && 
     !image_name %in% c("rocker/tidyverse", "rocker/geospatial", "rocker/verse")){
    cran_packages <- append(cran_packages, "devtools")
    pkg_names <- append(pkg_names, "devtools")
    if(requireNamespace("devtools"))
      package_versions <- append(package_versions, utils::packageVersion("devtools"))
    else
      #NOTE (TODO): the 'version' field is not yet evaluated or handled
      package_versions <- append(package_versions, "latest") 
  }

  #install those system dependencies  which are necessary
  if(!isTRUE(platform %in% .supported_platforms)){
    warning("The determined platform '", platform, "' is currently not supported for handling system dependencies. Therefore, they  cannot be installed by containerit.")
  }else if(length(pkg_names) > 0){

    
    #--- handle dependency exceptions (TODO: maybe handle with json config file?)--------------------
    
    # dependencies that can be left out
    # additional dependencies
    no_apt <- add_apt <- character(0) 
    # additional instructions that shall be appended -after- installing system requirements
    add_inst <- list()
    
    if("sf" %in% pkg_names){
      # sf-dependencies proj and gdal cannot be installed directly from apt get, because the available packages are outdated. 
      
      # The preferred way is to use the rocker/geospatial image where gdal and proj are pre-installed
      if(!image_name == "rocker/geospatial"){
        message("The dependent package simple features for R requires current versions from gdal, geos and proj that may not be available by standard apt-get.",
                "We recommend using the base image rocker/geospatial.")
        message("Docker will try to install GDAL 2.1.3 from source")
        
        add_apt <- append(add_apt, c("wget", "make"))
        add_inst <- append(add_inst, Workdir("/tmp/gdal"))
        add_inst <- append(add_inst, Run_shell(c("wget http://download.osgeo.org/gdal/2.1.3/gdal-2.1.3.tar.gz", 
                                                   "tar zxf gdal-2.1.3.tar.gz","cd gdal-2.1.3", 
                                                   "./configure","make",
                                                   "make install",
                                                   "ldconfig",
                                                   "rm -r /tmp/gdal"))
                           )
        
        #### or system.file("template_source_install_GDAL_PROJ",package ="containeRit"), 
        
        # TODO: # For Ubuntu images (not yet supported), there is a separate ppa available from ubuntuGIS (see https://github.com/edzer/sfr/blob/master/.travis.yml).
      }
    }
    
    # TODO: we may add some some more no-apt or analogue no-package exceptions here, but at the moment it won't be necessary
    if(image_name == "rocker/geospatial")
      #these packages are pre-installed
      no_apt <- append(no_apt, c("libproj-dev","libgeos-dev","gdal-bin"))
    
    #------------------------------------------------------------------------(end of section)
    
    # determine package dependencies (if applicable by given platform)
    pkg_dep <- .find_system_dependencies(pkg_names, platform = platform, package_version = package_versions, soft = soft)
    #workaround for issue https://github.com/r-hub/sysreqsdb/issues/22 TODO: remove if not needed anymore
    pkg_dep <- unlist(stringr::str_split(pkg_dep, pattern = " "))
    
    package_reqs <- append(package_reqs, pkg_dep)
    
    #some packages may not need to be installed, e.g. because they are pre-installed for a certain image
    package_reqs <- package_reqs[!package_reqs %in% no_apt]
    package_reqs <- append(package_reqs, add_apt)
    
    #remove dublicate system requirements
    package_reqs <- levels(as.factor(package_reqs))
    
    
    # if platform is debian and system dependencies need to be installed
    if(platform ==.debian_platform && length(package_reqs) > 0){
      commands <- "export DEBIAN_FRONTEND=noninteractive; apt-get -y update"
      install_command <- paste("apt-get install -y", paste(package_reqs, collapse = " \\\n\t"))
      commands <- append(commands, install_command)
      addInstruction(.dockerfile)  <- Run_shell(commands)
      
      if(length(add_inst)> 0)
        addInstruction(.dockerfile) <- add_inst
      # For using the exec form (??):
      #  Run("/bin/sh", params = c("-c","export","DEBIAN_FRONTEND=noninteractive"))) 
      # Run("apt-get", params = c("update", "-qq", "&&", "install", "-y" , package_reqs)))
    }

    # TODO:'mapping plaftorm > installation' command goes here, analogue to rsysreqs > https://github.com/r-hub/sysreqsdb/tree/master/platforms
  }
 

  
  #install cran packages
  if(length(cran_packages) > 0){
    params <- append(paste0("-r '", get_docker_cran_mirror(), "'"), cran_packages)
    run_install_cran <- Run("install2.r", params)
    addInstruction(.dockerfile) <- run_install_cran
  }
  
  if(length(github_packages) > 0){
    message("Github_packages: ", github_packages)
    addInstruction(.dockerfile) <- Run("installGithub.r", github_packages)
  }

  # TODO: install packages from other sources
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
  function(package, platform, soft, package_version, localFirst = TRUE) {
    #for more than one package:
    if(length(package)>1){
      out = mapply(function(pkg, version){
            .find_by_sysreqs_pkg(pkg, platform, soft, version, localFirst) 
      }, pkg = package, version = package_version)
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


# a helper method to reference github packages even if they are not installed
.githubRefs <- new.env()



.addGitHubRef <- function(pkg, ref){
  .githubRefs[[pkg]] <<- ref
}



#try for instance .getGitHubRef("sysrequs")
.getGitHubRef = function(pkg){
  # TODO: this method might be sub-optimal
  # as it takes additional information from devtools::session_info() instead of the given sessionInfo object
  # However, the information given by session_info is more precise and there should be no problem as long as the local installation matches the session info
  if(!is.null(.githubRefs[[pkg]]))
    return(.githubRefs[[pkg]])
  
  if(!requireNamespace(pkg))
    warning("Package ", pkg," cannot be loaded.")
  
  si <- devtools::session_info()
  sel <- si$packages$package == pkg
  source1 <- si$packages$source[sel]
  #try to determine github reference from devools
  if(stringr::str_detect(source1, "(?i)^GitHub \\(.*/.*@|#.*\\)$")) {
    source1 <- stringr::str_replace(source1,"(?i)^GitHub \\(",replacement = "")
    source1 <- stringr::str_replace(source1,"\\)$",replacement = "")  
    return(source1)
  }else{#alternatively, try with 'normal' sessioninfo (normally does not reference a commit)
    si <- sessionInfo()
    pkgs = c(si$otherPkgs, si$loadedOnly)
    repo <- pkgs[[pkg]]$GithubRepo
    uname <- pkgs[[pkg]]$GithubUsername
    ghr <- pkgs[[pkg]]$GithubRef
    ref = paste0(uname,"/",repo,"@",ghr)
    #nomally not so exact:
    warning("Exact reference of GitHub package ",ref," could not be determined.")
    return(ref)
  }
}

