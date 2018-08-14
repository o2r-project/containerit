# Copyright 2018 Opening Reproducible Research (https://o2r.info)

library(containerit)
context("baseimage helper functions")

test_that("installed packages can be read from a Docker image", {
  pkgs <- get_installed_packages(image = "rocker/geospatial:3.4.4")

  expect_equal(dim(pkgs), c(252,2))
  expect_true("sf" %in% pkgs$pkg)
  expect_true("maps" %in% pkgs$pkg)
  expect_true("rgeos" %in% pkgs$pkg)
  expect_true("maptools" %in% pkgs$pkg)
  expect_false("adehabitatLT" %in% pkgs$pkg)
  expect_false("cshapes" %in% pkgs$pkg)
})

test_that("installed packages are a data.frame with the image as an attribute", {
  .image <- "rocker/geospatial:3.4.4"
  pkgs <- get_installed_packages(image = .image)

  expect_s3_class(pkgs, "data.frame")
  expect_true("image" %in% names(attributes(pkgs)))
  expect_equal(.image, attributes(pkgs)$image)
})

test_that("list of installed packages can be filtered when creating a Dockerfile", {
  the_dockerfile <- dockerfile(from = "package_markdown/sfr/",
                   maintainer = "o2r",
                   image = "rocker/geospatial:3.4.4",
                   filter_baseimage_pkgs = TRUE)
  the_dockerfile_string <- toString(the_dockerfile)

  expect_true(object = any(grepl("# Packages skipped", x = the_dockerfile_string)), info = "Packages skipped are mentioned in a comment")
  expect_true(object = any(grepl("^#.*rgdal", x = the_dockerfile_string)), info = "rgdal")
  expect_true(object = any(grepl("^#.*spData", x = the_dockerfile_string)), info = "spData")

  unlink("package_markdown/sfr/nc1.*")
  unlink("package_markdown/sfr/*.html")
})
