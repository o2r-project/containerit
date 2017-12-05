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

test_that("The sf3 markdown file can be packaged", {
  skip("issue with libwlgeom-2.3 in sf")
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
  expected_file <- readLines("package_markdown/sf_vignette_Dockerfile")
  expected_file <- stringr::str_replace(string = expected_file, pattern = "###TEMPDIR###", replacement = dir)
  generated_file <- unlist(stringr::str_split(toString(df),"\n"))
  expect_equal(generated_file, expected_file)

  # here we can build and run the actual container to see if the resulting file is matching
  #expect_true(file.exists(file.path(dir, "sf3.html")))
  unlink(dir,recursive = TRUE)
})

test_that("The render command supports output directory", {
  cmd = CMD_Render(path = tempdir(), output_dir = "/the_directory")
  expect_equal(stringr::str_count(toString(cmd), 'output_dir = \\\\\\"/the_directory\\\\\\"'), 1)
  expect_equal(stringr::str_count(toString(cmd), 'output_file'), 0)
})

test_that("The render command supports output file", {
  cmd = CMD_Render(path = tempdir(), output_file = "myfile.html")
  expect_equal(stringr::str_count(toString(cmd), 'output_file = \\\\\\"myfile.html\\\\\\"'), 1)
  expect_equal(stringr::str_count(toString(cmd), 'output_dir'), 0)
})

test_that("The render command supports output directory and output file at the same time", {
  cmd = CMD_Render(path = tempdir(), output_dir = "/the_directory", output_file = "myfile.html")
  expect_equal(stringr::str_count(toString(cmd), 'output_dir = \\\\\\"/the_directory\\\\\\"'), 1)
  expect_equal(stringr::str_count(toString(cmd), 'output_file = \\\\\\"myfile.html\\\\\\"'), 1)
})

test_that("The file is automatically copied", {
  df_copy <- dockerfile(from = "package_markdown/markdowntainer-units/")
  expect_true(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "at least one Copy instruction")
  expect_s4_class(df@instructions[[length(df@instructions)]], "Copy")
})

test_that("File copying can be disabled with NA", {
  df_copy <- dockerfile(from = "package_markdown/markdowntainer-units/", copy = NA)
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("File copying can be disabled with NA_character", {
  df_copy <- dockerfile(from = "package_markdown/markdowntainer-units/", copy = NA_character_)
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("File copying can be disabled with NULL", {
  df_copy <- dockerfile(from = "package_markdown/markdowntainer-units/", copy = NULL)
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})
