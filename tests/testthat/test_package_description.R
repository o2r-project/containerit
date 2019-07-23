# Copyright 2018 Opening Reproducible Research (https://o2r.info)

context("Packaging a DESCRIPTION file")

test_that("version tag can be extracted from DESCRIPTION object", {
  description <- desc::desc(file = "package_description/DESCRIPTION")
  version <- getRVersionTag(description)
  expect_equal(version, "3.3.0")
})

test_that("a DESCRIPTION file can be packaged", {
  output <- capture_output(
    the_dockerfile <- dockerfile(from = "package_description/DESCRIPTION",
                                 maintainer = "o2r")
  )
  # write(the_dockerfile, file = "package_description/Dockerfile")
  expected_file <- readLines("package_description/Dockerfile")
  expect_equal(capture.output(print(the_dockerfile)), expected_file)
})

test_that("a DESCRIPTION object can be packaged", {
  description <- desc::desc(file = "package_description/DESCRIPTION")
  output <- capture_output(
    the_dockerfile <- dockerfile(from = description,
                                 maintainer = "o2r")
  )
  expected_file <- readLines("package_description/Dockerfile")
  expect_equal(capture.output(print(the_dockerfile)), expected_file)
})

test_that("a DESCRIPTION of an installed package can be packaged", {
  output <- capture_output(
    the_dockerfile <- dockerfile(from = desc::desc(package = "sf"),
                                 maintainer = "o2r")
  )
  # write(the_dockerfile, file = "package_description/Dockerfile.sf")

  expected_file <- readLines("package_description/Dockerfile.sf")
  expect_equal(capture.output(print(the_dockerfile)), expected_file)
})

test_that("the version of the packaged DESCRIPTION package can be installed", {
  output <- capture_output(
    the_dockerfile <- dockerfile(from = "package_description/DESCRIPTION",
                                 maintainer = "o2r",
                                 versioned_packages = TRUE)
  )
  # write(the_dockerfile, file = "package_description/Dockerfile.versioned")
  expected_file <- readLines("package_description/Dockerfile.versioned")
  expect_equal(capture.output(print(the_dockerfile)), expected_file)
})

test_that("the version of the packaged DESCRIPTION package can be installed for an installed package", {
  output <- capture_output(
    the_dockerfile <- dockerfile(from = desc::desc(package = "sf"),
                                 maintainer = "o2r",
                                 versioned_packages = TRUE)
  )
  # write(the_dockerfile, file = "package_description/Dockerfile.sf.versioned")
  expected_file <- readLines("package_description/Dockerfile.sf.versioned")
  expect_equal(capture.output(print(the_dockerfile)), expected_file)
})
