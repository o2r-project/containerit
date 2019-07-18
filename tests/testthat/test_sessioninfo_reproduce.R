# Copyright 2018 Opening Reproducible Research (https://o2r.info)

context("session reproduction")

# test-expressions: the first expression attaches a CRAN-package, the second expression loads one of the 'recommended'- packages without attaching it
# All libraries used must be locally installed prior to running this test!
expressions <- list(
  quote(library(sp)),
  quote(library(sysreqs)), # test for github package
  quote(codetools::showTree(quote(-3))) # test for attached package
)

test_that("a local sessionInfo() can be created ", {
  output <- capture_output(local_sessionInfo <- clean_session(expr = expressions))
  expect_s3_class(local_sessionInfo, "sessionInfo")
})

test_that("a sessionInfo can be reproduced with Docker", {
  skip_if_not(stevedore::docker_available())

  output <- capture_output({
    local_sessionInfo <- clean_session(expr = expressions)
    dockerfile_object <- dockerfile(local_sessionInfo)
    docker_tempimage_id <- docker_build(dockerfile_object)
  })

  #expect that image was created:
  client <- stevedore::docker_client()
  expect_true(docker_tempimage_id %in% client$image$list()$id)

  output <- capture_output({
    docker_sessionInfo <- extract_session_image(docker_tempimage_id, expressions)
  })
  expect_s3_class(docker_sessionInfo, "sessionInfo")

  # the same base packages are attached locally and in Docker
  expect_equal(local_sessionInfo$basePkgs, docker_sessionInfo$basePkgs)

  #expect that non-base packages are attached
  local_attached <- names(local_sessionInfo$otherPkgs)
  docker_attached <- names(docker_sessionInfo$otherPkgs)
  expect_true(all(local_attached %in% docker_attached))
  expect_true(all(docker_attached %in% local_attached))

  #expect that same base and non-base packages loaded via namespace
  local_loaded_packages <- Filter(
    f = function(x) { return(x$Package != "rstudioapi") },
    x = local_sessionInfo$loadedOnly)
  local_loaded <- names(local_loaded_packages)
  docker_loaded <- names(docker_sessionInfo$loadedOnly)
  expect_equal(local_loaded, docker_loaded)

  # the R versions are the same
  expect_equal(local_sessionInfo$R.version$major, docker_sessionInfo$R.version$major)
  expect_equal(local_sessionInfo$R.version$minor, docker_sessionInfo$R.version$minor)

  # TODO: the locales are the same
  #expect_equal(local_sessionInfo$locale, docker_sessionInfo$locale)

  #clean up: remove image
  client$image$remove(docker_tempimage_id, force = TRUE)
})
