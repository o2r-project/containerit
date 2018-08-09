# Copyright 2018 Opening Reproducible Research (https://o2r.info)

context("Package R markdown files")

test_that("A markdown file can be packaged (using units expample)", {
  skip("FIXME, works when run single, but not when run with full package check")

  the_dockerfile <- dockerfile(from = "package_markdown/units/",
                   maintainer = "Ted Tester",
                   image = getImageForVersion("3.3.2"),
                   copy = "script_dir",
                   cmd = CMD_Render("package_markdown/units/2016-09-29-plot_units.Rmd"))
  #write(the_dockerfile,"package_markdown/units_Dockerfile")
  expected_file <- readLines("package_markdown/units_Dockerfile")
  generated_file <- unlist(stringr::str_split(toString(the_dockerfile),"\n"))
  expect_equal(generated_file, expected_file)
})

test_that("The sf3 markdown file can be packaged", {
  skip("issue with libwlgeom-2.3 in sf")

  md_file <- system.file("doc/sf3.Rmd", package = "sf")
  dir <- file.path(tempdir(), "sf")
  dir.create(dir)
  tmpfile <- tempfile(tmpdir = dir, fileext = ".Rmd")
  file.copy(from = md_file, to = tmpfile)
  the_dockerfile <- dockerfile(dir,
                   maintainer = "o2r",
                   image = "rocker/geospatial",
                   copy = "script_dir",
                   cmd = CMD_Render(dir, output_dir = "/output"))
  #write(the_dockerfile,"package_markdown/sf_vignette_Dockerfile")
  expected_file <- readLines("package_markdown/sf_vignette_Dockerfile")
  expected_file <- stringr::str_replace(string = expected_file, pattern = "###TEMPDIR###", replacement = dir)
  generated_file <- unlist(stringr::str_split(toString(the_dockerfile),"\n"))
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

test_that("The file is copied", {
  df_copy <- dockerfile(from = "package_markdown/units/", copy = "script")
  expect_true(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "at least one Copy instruction")
})

test_that("File copying is disabled by default", {
  df_copy <- dockerfile(from = "package_markdown/units/")
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("File copying can be disabled with NA/NA_character", {
  df_copy <- dockerfile(from = "package_markdown/units/", copy = NA_character_)
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
  df_copy2 <- dockerfile(from = "package_markdown/units/", copy = NA_character_)
  expect_false(object = any(sapply(df_copy2@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("File copying can be disabled with NULL", {
  df_copy <- dockerfile(from = "package_markdown/units/", copy = NULL)
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("Packaging fails if dependency is missing and predetection is disabled", {
  skip_on_cran() # cannot remove packages on CRAN
  skip("FIXME need a better way to test then using plm")
  if (requireNamespace("plm", quietly = TRUE)) {
    remove.packages(pkgs = c("plm"))
  }
  expect_error(dockerfile(from = "package_markdown/spacetime/", predetect = FALSE), "Failed to execute")
})

test_that("Packaging works if dependency is missing in the base image and predetection is enabled", {
  skip("error removing the package during running tests...")
  skip_on_cran() # cannot remove packages on CRAN
  if (requireNamespace("plm", quietly = TRUE)) {
    remove.packages(pkgs = c("plm"))
  }
  # this will re-install the package plm again:
  the_dockerfile <- dockerfile(from = "package_markdown/spacetime/", maintainer = "o2r", predetect = TRUE)
  expected_file <- readLines("package_markdown/spacetime/Dockerfile")
  generated_file <- unlist(stringr::str_split(toString(the_dockerfile),"\n"))
  expect_equal(generated_file, expected_file)
  expect_true(object = any(grepl("\"plm\"", x = toString(the_dockerfile))), info = "Packages missing in the base image are detected")
})
