# Copyright 2018 Opening Reproducible Research (https://o2r.info)

library("containerit")
context("baseimage helper functions")

test_that("installed packages can be read from a Docker image", {
  skip_on_appveyor()
  skip_on_cran()

  output <- capture_output(pkgs <- get_installed_packages(image = "rocker/geospatial:3.4.4"))

  expect_equal(dim(pkgs), c(257,2))
  expect_true("sf" %in% pkgs$pkg)
  expect_true("maps" %in% pkgs$pkg)
  expect_true("rgeos" %in% pkgs$pkg)
  expect_true("maptools" %in% pkgs$pkg)
  expect_false("adehabitatLT" %in% pkgs$pkg)
  expect_false("cshapes" %in% pkgs$pkg)
})

test_that("installed packages are a data.frame with the image as an attribute", {
  skip_on_appveyor()
  skip_on_cran()

  .image <- "rocker/geospatial:3.4.4"
  output <- capture_output(pkgs <- get_installed_packages(image = .image))

  expect_s3_class(pkgs, "data.frame")
  expect_true("image" %in% names(attributes(pkgs)))
  expect_equal(.image, attributes(pkgs)$image)
})

test_that("list of installed packages can be filtered when creating a Dockerfile", {
  skip_on_appveyor()
  skip_on_cran()

  output <- capture_output(the_dockerfile <- dockerfile(from = "package_markdown/sfr/",
                   maintainer = "o2r",
                   image = "rocker/geospatial:3.4.4",
                   filter_baseimage_pkgs = TRUE))
  the_dockerfile_string <- toString(the_dockerfile)

  expect_true(object = any(grepl("# CRAN packages skipped", x = the_dockerfile_string)), info = "Packages skipped are mentioned in a comment")
  expect_true(object = any(grepl("^#.*units", x = the_dockerfile_string)), info = "units")
  expect_true(object = any(grepl("^#.*Rcpp", x = the_dockerfile_string)), info = "Rcpp")

  unlink("package_markdown/sfr/nc1.*")
  unlink("package_markdown/sfr/*.html")
  unlink("package_markdown/sfr/sf2_files", recursive = TRUE)
})

test_that("filtered list of installed packages does not filter GitHub packages", {
  skip_on_appveyor()
  skip_on_cran()

  # created sessionInfo file:
  # $  docker run --rm -it -v $(pwd):/data rocker/geospatial:3.5.1 R
  # R> devtools::install_github("o2r-project/containerit")
  # R> devtools::install_github("tidyverse/ggplot2", ref ="v3.0.0")
  # R> library("ggplot2")
  # R> sessionInfo <- sessionInfo()
  # R> save(sessionInfo, file = "/data/sessionInfo.RData")

  output <- capture_output(the_dockerfile <- dockerfile(from = "github/sessionInfo1.RData",
                               maintainer = "o2r",
                               image = "rocker/geospatial:3.5.1",
                               filter_baseimage_pkgs = TRUE))
  the_dockerfile_string <- toString(the_dockerfile)

  expect_false(object = any(grepl("^# CRAN packages skipped.*ggplot", x = the_dockerfile_string)), info = "ggplot is not skipped")
  expect_true(object = any(grepl("^RUN.*installGithub.*tidyverse/ggplot2", x = the_dockerfile_string)), info = "ggplot is in installGithub command")
})

test_that("filtered list of installed packages is alphabetical", {
  skip_on_appveyor()
  skip_on_cran()

  output <- capture_output(the_dockerfile <- dockerfile(from = "github/sessionInfo1.RData",
                               maintainer = "o2r",
                               image = "rocker/geospatial:3.5.1",
                               filter_baseimage_pkgs = TRUE))
  the_dockerfile_string <- toString(the_dockerfile)

  expect_true(object = any(grepl("^# CRAN packages skipped.*assertthat.*curl.*digest.*rlang.*withr", x = the_dockerfile_string)))
})
