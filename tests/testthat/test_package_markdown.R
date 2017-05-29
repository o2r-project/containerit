# Copyright 2017 Opening Reproducible Research (http://o2r.info)

library(testthat)
library(containerit)
context("Package R markdown files")


test_that("A markdown file can be packaged (using markdowntainer-units-expample)", {
  df <- dockerfile(from = "package_markdown/markdowntainer-units/",
                   maintainer = "Ted Tester",
                   r_version = "3.3.2",
                   copy = "script_dir",
                   cmd = CMD_Render("package_markdown/markdowntainer-units/2016-09-29-plot_units.Rmd"))
  #write(df, "package_markdown/units_Dockerfile")
  expected_file <- readLines("package_markdown/units_Dockerfile")
  generated_file <- unlist(stringr::str_split(toString(df),"\n"))
  expect_equal(generated_file, expected_file)
})


test_that("A sf markdown file can be packaged", {
  md_file <- system.file("doc/sf3.Rmd", package = "sf")
  dir <- file.path(tempdir(), "sf")
  dir.create(dir)
  tmpfile <- tempfile(tmpdir = dir, fileext = ".Rmd")
  file.copy(from = md_file, to = tmpfile)
  df <- dockerfile(dir,
                   maintainer = "matthiashinz",
                   image = "rocker/geospatial",
                   copy = "script_dir",
                   cmd = CMD_Render(dir, output_dir = "/output"))
  #write(df, "package_markdown/sf_vignette_Dockerfile")
  expected_file = readLines("package_markdown/sf_vignette_Dockerfile")
  generated_file <- unlist(stringr::str_split(toString(df),"\n"))
  expect_equal(generated_file, expected_file)
  # here we can build and run the actual container to see if the resulting file is matching
  #expect_true(file.exists(file.path(dir, "sf3.html")))
  unlink(dir,recursive = TRUE)
})

