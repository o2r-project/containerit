#pkgs list of packages as returned by sessionInfo
.create_run_install <- function(pkgs) {
  #create RUN expressions
  package_reqs <- character(0)
  cran_packages <-
    github_packages <- local_packages <- other_packages <- character(0)
  
  sapply(pkgs,
         function(pkg) {
           if ("Package" %in% names(pkg))
             name <- pkg$Package
           else
             stop("Package name cannot be dertermined for ", pkg) #should hopefully never occure
           
           if ("Priority" %in% names(pkg) &&
               stringr::str_detect(pkg$Priority, "(?i)base")) {
             #packages with these priorities are normally included and don't need to be installed; do nothing
             return()
           }
           pkg_dep <- find_system_dependencies(name, pkg$Version)
           package_reqs <<- append(package_reqs, pkg_dep)
           
           #check if package come from CRAN (alternatively you may use devtools::session_info)
           if ("Repository" %in% names(pkg) &&
               stringr::str_detect(pkg$Repository, "(?i)CRAN"))
           {
             cran_packages <<- append(cran_packages, pkg$Package)
             return()
             ##TODO: handle github and outher package sources
           } else
             warning(
               "Failed to identify source for package ",
               pkg$Package,
               ". Therefore the package cannot be installed in the docker image."
             )
         })
  
  run_instructions <- list()
  
  
  #install system dependencies
  if (length(package_reqs) > 0) {
    run_instructions <-
      append(run_instructions,  Run("apt-get", params = c("update", "-qq")))
    package_reqs <-
      levels(as.factor(package_reqs)) #remove dublicates
    run_instructions <-
      append(run_instructions,  Run("apt-get", params = c("install", "-y" , package_reqs)))
  }
  
  #install cran packages
  params <- append(paste0("-r '", get_docker_cran_mirror(), "'"), cran_packages)
  run_install_cran <- Run("install2.r", params)
  run_instructions <- append(run_instructions, run_install_cran)
  return(run_instructions)
}

.find_system_dependencies <-
  function(package,
           version = utils::packageVersion(package),
           platform = "linux-x86_64-debian-gcc",
           method = "sysreq-api") {
    if (method == "sysreq-package")
      return(.find_by_sysreqs_pkg(
        package = package,
        version = version,
        platform = platform
      ))
    if (method == "sysreq-api")
      return(.find_by_sysreqs_api(package = package, platform = platform))
  }

.find_by_sysreqs_pkg <-
  function(package,
           version = utils::packageVersion(package),
           localFirst = TRUE,
           platform = "linux-x86_64-debian-gcc") {
    sysreqs <- character(0)
    if (localFirst) {
      path <- find.package(package, quiet = TRUE)
      if (is.null(path) ||
          length(path) == 0 || utils::packageVersion(package) != version) {
        message(
          "No package DESCRIPTION found locally for package '",
          package,
          "', version '",
          version,
          "' ."
        )
      } else{
        sysreqs <- sysreqs::sysreqs(file.path(path, "DESCRIPTION"), platform)
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
      sysreqs <- sysreqs(temp, platform)
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
  function(package, platform = "linux-x86_64-debian-gcc") {
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
