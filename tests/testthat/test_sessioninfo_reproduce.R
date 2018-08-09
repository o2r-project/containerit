# Copyright 2018 Opening Reproducible Research (https://o2r.info)

library(containerit)

requireNamespace("rgdal")
requireNamespace("proj4")
requireNamespace("sp")
requireNamespace("codetools")

context("session reproduction")

# test-expressions: the first expression attaches a CRAN-package, the second expression loads one of the 'recommended'- packages without attaching it
# All libraries used must be locally installed prior to running this test!
expressions <- list(
  #quote(library(rgdal)),
  #quote(library(proj4)),
  quote(library(sp)),
  quote(library(sysreqs)), # test for github package
  quote(codetools::showTree(quote(-3))) # test for attached package
)

local_sessionInfo <- NULL
docker_sessionInfo <- NULL
dockerfile_object <- NULL

## optains a session info from an R session executed in docker given expression expr and a docker image with R installed
#  This method currently supports only expressions as an input (they should not be too long and complex).
#  If the method should also execute complete scripts and optain the sessionInfo, it would have to be re-written according to optain_localSessionInfo.
#  A temporary R script must be mounted. And then exectuted inside the container.
#  As this function was only created for test purposes in order to compare sessionInfos, this feature is out of scope at the moment.
obtain_dockerSessionInfo <- function(docker_image,
           expr = c(),
           vanilla = FALSE,
           container_dir = "/tmp",
           local_dir = tempfile(pattern = "dir"),
           deleteTempfiles = TRUE) {
    result = tryCatch({
      # for testing:
      # docker_image <- "rocker/geospatial:3.4.4"

      #create local temporary directory
      dir.create(local_dir)
      if (!dir.exists(local_dir))
        stop("Unable to locate temporary directory: ", local_dir)

      #rdata file to which session info shall be written
      container_tempfile =  file.path(container_dir, "capture.Rdata")
      local_docker_tempfile = file.path(local_dir, "capture.Rdata")

      expr <- append(expr, containerit:::.writeSessionInfoExp(container_tempfile))
      #convert to cmd parameters
      expr <- .exprToParam(expr)

      cmd <- c("R")
      if (vanilla) {
        cmd <- append(cmd, "--vanilla")
      }
      cmd <- append(cmd, expr)

      futile.logger::flog.info("Running R in container to obtain a session info using image %s and command %s",
                               docker_image,
                               paste(cmd, collapse = " "))

      client <- stevedore::docker_client()
      container <- client$container$run(image = docker_image,
                                        cmd = cmd,
                                        host_config = list(binds = c(paste0(local_dir, ":", container_dir))),
                                        name = "containerit_capturer")

      if (!file.exists(local_docker_tempfile))
        stop("Sessioninfo was not written to file (file missing): ",
             local_docker_tempfile)

      futile.logger::flog.info("Wrote sessioninfo from Docker to %s", local_docker_tempfile)
      load(local_docker_tempfile)
      #clean up
      if (deleteTempfiles)
        unlink(local_dir, recursive = TRUE)
      container$container$remove()

      get("info")
    }, error = function(e) {
      cat("Error obtaining session info:", toString(e), "\n")
      NULL
    }, finally = {
      #
    })

    return(result)
  }

test_that("a local sessionInfo() can be created ", {
  local_sessionInfo <<- containerit:::obtain_localSessionInfo(expr = expressions, vanilla = TRUE)
  expect_s3_class(local_sessionInfo, "sessionInfo")
})

test_that("a sessionInfo can be reproduced with Docker", {
  skip_on_cran()
  skip_on_travis()
  skip_if_not(stevedore::docker_available())

  if (is.null(local_sessionInfo))
    skip("previous test failed (missing objects to continue)")

  dockerfile_object <- dockerfile(local_sessionInfo)
  docker_tempimage_id <- containerit:::create_localDockerImage(dockerfile_object)

  #expect that image was created:
  client <- stevedore::docker_client()
  expect_true(docker_tempimage_id %in% client$image$list()$id)

  docker_sessionInfo <<- obtain_dockerSessionInfo(docker_tempimage, expressions, vanilla = TRUE)
  skip_if(is.null(docker_sessionInfo))

  #clean up: remove image
  client$image$remove(docker_tempimage_id)
})

test_that("the same base packages are attached locally and in Docker", {
  skip_on_cran()
  skip_on_travis()
  skip_if_not(!is.null(docker_sessionInfo))

  if(is.null(docker_sessionInfo))
    skip("previous test failed (missing objects to continue)")

  #expect that same base packages are attached
  expect_true(all(
    local_sessionInfo$basePkgs %in% docker_sessionInfo$basePkgs
  ))
  expect_true(all(
    docker_sessionInfo$basePkgs %in% local_sessionInfo$basePkgs
  ))
})

test_that("the same other packages are attached locally and in Docker ", {
  skip_on_cran()
  skip_on_travis()
  skip_if_not(!is.null(docker_sessionInfo))

  #expect that non-base packages are attached
  local_attached <- names(local_sessionInfo$otherPkgs)
  docker_attached <- names(docker_sessionInfo$otherPkgs)
  expect_true(all(local_attached %in% docker_attached))
  expect_true(all(docker_attached %in% local_attached))
  #check versions
  local_versions <-
    sapply(local_sessionInfo$otherPkgs, function(x)
      x$Version)
  docker_versions <-
    sapply(docker_sessionInfo$otherPkgs, function(x)
      x$Version)
  expect_equal(local_versions, docker_versions)
})

test_that("the packages are loaded via Namespace locally and in Docker (requires updated local packages)", {
  skip_on_cran()
  skip_on_travis()
  skip_if_not(!is.null(docker_sessionInfo))

  # FIXME remove rstudioapi from the local packages
  local_loaded_packages <- Filter(
    f = function(x) { return(x$Package != "rstudioapi") },
    x = local_sessionInfo$loadedOnly)

  #expect that same base and non-base packages loaded via namespace
  local_loaded <- names(local_loaded_packages)
  docker_loaded <- names(docker_sessionInfo$loadedOnly)
  expect_true(all(local_loaded %in% docker_loaded))
  expect_true(all(docker_loaded %in% local_loaded))

  local_versions <- sapply(local_loaded_packages, function(x) x$Version)
  docker_versions <- sapply(docker_sessionInfo$loadedOnly, function(x) x$Version)
  expect_equal(local_versions, docker_versions)
})

test_that("the R versions are the same ", {
  skip_on_cran()
  skip_on_travis()
  skip_if_not(!is.null(docker_sessionInfo))

  #expect that same base and non-base packages loaded via namespace
  expect_equal(local_sessionInfo$R.version$major,
               docker_sessionInfo$R.version$major)
  expect_equal(local_sessionInfo$R.version$minor,
               docker_sessionInfo$R.version$minor)
})

test_that("the locales are the same ", {
  skip_on_cran()
  skip_on_travis()
  skip_if_not(!is.null(docker_sessionInfo))

  skip("not implemented yet")
  #expect_equal(local_sessionInfo$locale, docker_sessionInfo$locale)
})

# manual comparison
if (FALSE) {
  cat("\nlocal sessionInfo: \n\n")
  print(local_sessionInfo)
  cat("\n------------------------------------")
  cat("\nreproduced sessionInfo in docker: \n\n")
  print(docker_sessionInfo)

  cat("\nDockerfile: \n\n")
  cat(paste(format(dockerfile_object), collapse = "\n"))
}
