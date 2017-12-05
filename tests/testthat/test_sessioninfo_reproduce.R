# Copyright 2017 Opening Reproducible Research (http://o2r.info)

# This script may take a few minutes. Disable this test, e.g. by renaming the file, in order to speed-up the testing

library(containerit)

requireNamespace("rgdal")
requireNamespace("proj4")
requireNamespace("sp")
requireNamespace("codetools")

context("session-reproduction")

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
           docker_tempdir = "/tmp/containerit_temp",
           local_tempdir = tempfile(pattern = "dir"),
           deleteTempfiles = TRUE) {
    result = tryCatch({
      #create local temporary directory
      dir.create(local_tempdir)
      if (!dir.exists(local_tempdir))
        stop("Unable to locate temporary directory: ", local_tempdir)

      #mount option
      volume_opt = c("-v", paste0(local_tempdir, ":", docker_tempdir))

      #rdata file to which session info shall be written
      docker_tempfile =  paste0(docker_tempdir, "/", "rdata")
      local_docker_tempfile = file.path(local_tempdir, "rdata")
      #cat(writeExp(docker_tempfile))
      expr <- append(expr, .writeSessionInfoExp(docker_tempfile))
      #convert to cmd parameters
      expr <- .exprToParam(expr)

      cmd <- c("R")
      if (vanilla) {
        cmd <- append(cmd, "--vanilla")
      }
      cmd <- append(cmd, expr)
      futile.logger::flog.info("Creating R session in Docker with the following arguments:\n\t",
                               "docker run %s %s %s",
                               paste(volume_opt, collapse = " "),
                               docker_image,
                               paste(cmd, collapse = " "))


      container <- harbor::docker_run(
        harbor::localhost,
        image = docker_image,
        cmd = cmd ,
        docker_opts = volume_opt
      )

      if (harbor::container_running(container))
        stop("Unexpected behavior: The container is still running!")

      harbor::container_rm(container)

      if (!file.exists(local_docker_tempfile))
        stop("Sessioninfo was not written to file (it does not exist): ",
             local_docker_tempfile)

      futile.logger::flog.info("Wrote sessioninfo from Docker to %s", local_docker_tempfile)
      load(local_docker_tempfile)
      #clean up
      if (deleteTempfiles)
        unlink(local_tempdir, recursive = TRUE)
      get("info")
    }, error = function(e) {
      cat("Error obtaining session infor via harbor:", toString(e), "\n")
      NULL
    }, finally = {
      #
    })

    return(result)
  }

test_that("a local sessionInfo() can be created ", {
  local_sessionInfo <<- obtain_localSessionInfo(expr = expressions, vanilla = TRUE)
  expect_s3_class(local_sessionInfo, "sessionInfo")
})

test_that("a sessionInfo can be reproduced with docker", {
  skip_on_cran()
  skip_on_travis()

  if(is.null(local_sessionInfo))
    skip("previous test failed (missing objects to continue)")

  dockerfile_object <<- dockerfile(local_sessionInfo)
  docker_tempimage <- create_localDockerImage(dockerfile_object, no_cache = FALSE)

  #expect that image was created:
  expect_match(
    harbor::docker_cmd(
      harbor::localhost,
      "images",
      docker_tempimage,
      capture_text = TRUE
    ), docker_tempimage)

  docker_sessionInfo <<- obtain_dockerSessionInfo(docker_tempimage, expressions, vanilla = TRUE)
  skip_if_not(!is.null(docker_sessionInfo))
  #clean up: remove image
  harbor::docker_cmd(harbor::localhost, "rmi", docker_tempimage)
})

test_that("the same base packages are attached locally and in Docker ", {
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

  #expect that same base and non-base packages loaded via namespace
  local_loaded <- names(local_sessionInfo$loadedOnly)
  docker_loaded <- names(docker_sessionInfo$loadedOnly)
  expect_true(all(local_loaded %in% docker_loaded))
  expect_true(all(docker_loaded %in% local_loaded))

  local_versions <-
    sapply(local_sessionInfo$loadedOnly, function(x)
      x$Version)
  docker_versions <-
    sapply(docker_sessionInfo$loadedOnly, function(x)
      x$Version)
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

#visual comparison
if(FALSE) {
  cat("\nlocal sessionInfo: \n\n")
  print(local_sessionInfo)
  cat("\n------------------------------------")
  cat("\nreproduced sessionInfo in docker: \n\n")
  print(docker_sessionInfo)

  cat("\nDockerfile: \n\n")
  cat(paste(format(dockerfile_object), collapse = "\n"))
}
