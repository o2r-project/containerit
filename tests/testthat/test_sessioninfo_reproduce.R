# Copyright 2016 Opening Reproducible Research (http://o2r.info)

# This script may take a few minutes. Disable this test, e.g. by renaming the file, in order to speed-up the testing

library(containeRit)

requireNamespace("rgdal")
requireNamespace("proj4")
requireNamespace("sp")
requireNamespace("codetools")

context("session-reproduction")

#test-expressions: the first expression attaches a CRAN-package, the second expression loads one of the 'recommended'- packages without attaching it
#Any library in use needs to be locally installed prior to running this test
expressions <- list(quote(library(rgdal)),
                    quote(library(proj4)),
                    quote(library(sp)),
                    quote(codetools::showTree(quote(-3))))

local_sessionInfo <- NULL

docker_sessionInfo <- NULL
dockerfile_object <- NULL

test_that("a local sessionInfo() can be created ", {
  local_sessionInfo <<-
    obtain_localSessionInfo(expr = expressions, vanilla = TRUE)
  expect_s3_class(local_sessionInfo, "sessionInfo")
})

test_that("a sessionInfo can be reproduced with docker", {
  dockerfile_object <<- dockerfile(local_sessionInfo)
  docker_tempimage <-
    create_localDockerImage(dockerfile_object, no_cache = FALSE)
  
  #expect that image was created:
  expect_equal(length(
    harbor::docker_cmd(
      harbor::localhost,
      "images",
      docker_tempimage,
      capture_text = TRUE
    )
  ), 2)
  
  docker_sessionInfo <<-
    obtain_dockerSessionInfo(docker_tempimage, expressions, vanilla = TRUE)
  #clean up
  harbor::docker_cmd(harbor::localhost, "rmi", docker_tempimage)
})

test_that("the same base packages are attached locally and in Docker ", {
  #expect that same base packages are attached
  expect_true(all(
    local_sessionInfo$basePkgs %in% docker_sessionInfo$basePkgs
  ))
  expect_true(all(
    docker_sessionInfo$basePkgs %in% local_sessionInfo$basePkgs
  ))
})

test_that("the same other packages are attached locally and in Docker ", {
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

test_that("the packages are loaded via Namespace locally and in Docker ", {
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
  #expect that same base and non-base packages loaded via namespace
  expect_equal(local_sessionInfo$R.version$major,
               docker_sessionInfo$R.version$major)
  expect_equal(local_sessionInfo$R.version$minor,
               docker_sessionInfo$R.version$minor)
})


test_that("the locales are the same ", {
  message("TODO: session locales are currently not reproduced.")
  #expect_equal(local_sessionInfo$locale, docker_sessionInfo$locale)
})


#visual comparism
cat("\nlocal sessionInfo: \n\n")
print(local_sessionInfo)
cat("\n------------------------------------")
cat("\nreproduced sessionInfo in docker: \n\n")
print(docker_sessionInfo)

cat("\nDockerfile: \n\n")
cat(paste(format(dockerfile_object), collapse = "\n"))