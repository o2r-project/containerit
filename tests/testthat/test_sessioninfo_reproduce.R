# Copyright 2018 Opening Reproducible Research (https://o2r.info)

requireNamespace("sp")
requireNamespace("codetools")

context("session reproduction")

# test-expressions: the first expression attaches a CRAN-package, the second expression loads one of the 'recommended'- packages without attaching it
# All libraries used must be locally installed prior to running this test!
expressions <- list(
  quote(library(sp)),
  quote(library(sysreqs)), # test for github package
  quote(codetools::showTree(quote(-3))) # test for attached package
)

local_sessionInfo <- NULL
docker_sessionInfo <- NULL
dockerfile_object <- NULL

test_that("a local sessionInfo() can be created ", {
  output <- capture_output(local_sessionInfo <<- clean_session(expr = expressions))
  expect_s3_class(local_sessionInfo, "sessionInfo")
})

test_that("a sessionInfo can be reproduced with Docker", {
  skip_if_not(stevedore::docker_available())

  output <- capture_output({
    dockerfile_object <- dockerfile(local_sessionInfo)
    docker_tempimage_id <- docker_build(dockerfile_object)
  })

  #expect that image was created:
  client <- stevedore::docker_client()
  expect_true(docker_tempimage_id %in% client$image$list()$id)

  output <- capture_output({
    docker_sessionInfo <<- extract_session_image(docker_tempimage_id, expressions)
  })

  #clean up: remove image
  client$image$remove(docker_tempimage_id, force = TRUE)
})

test_that("the same base packages are attached locally and in Docker", {
  skip_if_not(stevedore::docker_available())
  expect_false(is.null(docker_sessionInfo))

  expect_equal(local_sessionInfo$basePkgs, docker_sessionInfo$basePkgs)
})

test_that("the same other packages are attached locally and in Docker ", {
  skip_if_not(stevedore::docker_available())
  expect_false(is.null(docker_sessionInfo))

  #expect that non-base packages are attached
  local_attached <- names(local_sessionInfo$otherPkgs)
  docker_attached <- names(docker_sessionInfo$otherPkgs)
  expect_true(all(local_attached %in% docker_attached))
  expect_true(all(docker_attached %in% local_attached))

  #check versions > no can do, because the local R does not have the same MRAN snapshot
  #local_versions <- sapply(local_sessionInfo$otherPkgs, function(x) x$Version)
  #docker_versions <- sapply(docker_sessionInfo$otherPkgs, function(x) x$Version)
  #expect_equal(local_versions, docker_versions)
})

test_that("the packages are loaded via Namespace locally and in Docker (requires updated local packages)", {
  skip_if_not(stevedore::docker_available())

  # remove rstudioapi from the local packages when running test manually
  local_loaded_packages <- Filter(
    f = function(x) { return(x$Package != "rstudioapi") },
    x = local_sessionInfo$loadedOnly)

  #expect that same base and non-base packages loaded via namespace
  local_loaded <- names(local_loaded_packages)
  docker_loaded <- names(docker_sessionInfo$loadedOnly)
  expect_equal(local_loaded, docker_loaded)

  #local_versions <- sapply(local_loaded_packages, function(x) x$Version)
  #docker_versions <- sapply(docker_sessionInfo$loadedOnly, function(x) x$Version)
  #expect_equal(local_versions, docker_versions)
})

test_that("the R versions are the same ", {
  skip_if_not(stevedore::docker_available())

  expect_equal(local_sessionInfo$R.version$major, docker_sessionInfo$R.version$major)
  expect_equal(local_sessionInfo$R.version$minor, docker_sessionInfo$R.version$minor)
})

test_that("the locales are the same ", {
  skip_if_not(stevedore::docker_available())

  skip("not implemented yet")
  #expect_equal(local_sessionInfo$locale, docker_sessionInfo$locale)
})
