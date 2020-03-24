# Copyright 2018 Opening Reproducible Research (https://o2r.info)

context("Base image helper functions")

test_that("Installed packages can be read from a Docker image", {
  skip_if_not(stevedore::docker_available())

  output <- capture_output(pkgs <- get_installed_packages(image = "rocker/geospatial:3.4.4"))

  expect_equal(dim(pkgs), c(257,2))
  expect_true("sf" %in% pkgs$pkg)
  expect_true("maps" %in% pkgs$pkg)
  expect_true("rgeos" %in% pkgs$pkg)
  expect_true("maptools" %in% pkgs$pkg)
  expect_false("adehabitatLT" %in% pkgs$pkg)
  expect_false("cshapes" %in% pkgs$pkg)
})

test_that("Installed packages are a data.frame with the image as an attribute", {
  skip_if_not(stevedore::docker_available())

  .image <- "rocker/geospatial:3.4.4"
  output <- capture_output(pkgs <- get_installed_packages(image = .image))

  expect_s3_class(pkgs, "data.frame")
  expect_true("image" %in% names(attributes(pkgs)))
  expect_equal(.image, attributes(pkgs)$image)
})

packages_df <- data.frame(name = c("sp", "ggplot2", "rgdal", "coxrobust"),
                          version = c("10.10.10", "tidyverse/ggplot2@abcdef", "3", "0"),
                          source = c("CRAN", "github", "CRAN", "CRAN"),
                          stringsAsFactors = FALSE)

test_that("List of installed packages can be filtered when creating a Dockerfile", {
  skip_if_not(stevedore::docker_available())

  output <- capture_output(the_dockerfile <- dockerfile(from = "package_markdown/sfr/",
                   maintainer = "o2r",
                   image = "rocker/geospatial:3.6.0",
                   filter_baseimage_pkgs = TRUE))
  the_dockerfile_string <- toString(the_dockerfile)

  expect_true(object = any(grepl("# CRAN packages skipped.*sf, sp, units", x = the_dockerfile_string)), info = "Packages skipped are mentioned in a comment")
  expect_true(object = any(grepl("^#.*units", x = the_dockerfile_string)), info = "units")

  unlink("package_markdown/sfr/nc1.*")
  unlink("package_markdown/sfr/*.html")
  unlink("package_markdown/sfr/sf2_files", recursive = TRUE)
})

test_that("Filtered list of installed packages does not filter GitHub packages", {
  skip_if_not(stevedore::docker_available())

  output <- capture_output(the_dockerfile <- dockerfile(from = packages_df,
                               maintainer = "o2r",
                               image = "rocker/geospatial:3.5.1",
                               filter_baseimage_pkgs = TRUE))
  the_dockerfile_string <- toString(the_dockerfile)

  expect_false(object = any(grepl("^# CRAN packages skipped.*ggplot", x = the_dockerfile_string)), info = "ggplot is not skipped")
  expect_true(object = any(grepl("^RUN.*installGithub.*tidyverse/ggplot2", x = the_dockerfile_string)), info = "ggplot is in installGithub command")
})

test_that("Filtered list of installed packages is alphabetical", {
  skip_if_not(stevedore::docker_available())

  output <- capture_output(the_dockerfile <- dockerfile(from = packages_df,
                               maintainer = "o2r",
                               image = "rocker/geospatial:3.5.1",
                               filter_baseimage_pkgs = TRUE))
  expect_true(object = any(grepl("^# CRAN packages skipped.*rgdal, sp", x = toString(the_dockerfile))))
})

test_that("System dependencies of filtered packages are not installed", {
  skip_if_not(stevedore::docker_available())

  output <- capture_output(the_dockerfile <- dockerfile(from = packages_df,
                                                        maintainer = "o2r",
                                                        image = "rocker/geospatial:3.6.0",
                                                        filter_baseimage_pkgs = TRUE))
  expect_false(object = any(grepl(".*gdal-bin.*", x = toString(the_dockerfile))))
})
