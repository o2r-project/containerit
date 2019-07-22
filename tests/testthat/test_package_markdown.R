# Copyright 2018 Opening Reproducible Research (https://o2r.info)

context("Package R markdown files")

# need to list boxoffice in DESCRIPTION for R CMD check
capture_warnings(ifelse(require("boxoffice"), remove.packages("boxoffice"), function() {}))

test_that("A markdown file can be packaged (using units expample)", {
  skip("Results differ from execution with all tests and single/manual execution")
  output <- capture_output({
    the_dockerfile <- dockerfile(from = "package_markdown/units/",
                   maintainer = "Ted Tester",
                   image = "rocker/verse:3.5.2",
                   copy = "script_dir",
                   cmd = CMD_Render("package_markdown/units/2016-09-29-plot_units.Rmd"))
  })
  #write(the_dockerfile,"package_markdown/units_Dockerfile")
  cat("\n"); print(the_dockerfile)
  expected_file <- readLines("package_markdown/units_Dockerfile")
  generated_file <- capture.output(print(the_dockerfile))
  expect_equal(generated_file, expected_file)
})

test_that("The sf3 markdown file can be packaged", {
  md_file <- system.file("doc/sf3.Rmd", package = "sf")
  dir <- file.path(tempdir(), "sf")
  dir.create(dir)
  tmpfile <- tempfile(tmpdir = dir, fileext = ".Rmd")
  file.copy(from = md_file, to = tmpfile)
  output <- capture_output(
    the_dockerfile <- dockerfile(dir,
                   maintainer = "o2r",
                   image = "rocker/geospatial",
                   copy = "script_dir",
                   cmd = CMD_Render(dir, output_dir = "/output"))
  )
  #write(the_dockerfile,"package_markdown/sf_vignette_Dockerfile")
  expected_file <- readLines("package_markdown/sf_vignette_Dockerfile")
  expected_file <- stringr::str_replace(string = expected_file, pattern = "###TEMPDIR###", replacement = dir)
  generated_file <- unlist(stringr::str_split(toString(the_dockerfile),"\n"))
  expect_equal(generated_file, expected_file)

  # here we can build and run the actual container to see if the resulting file is matching
  #expect_true(file.exists(file.path(dir, "sf3.html")))
  unlink(dir, recursive = TRUE)
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
  output <- capture_output(df_copy <- dockerfile(from = "package_markdown/units/", copy = "script"))
  expect_true(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "at least one Copy instruction")
})

test_that("File copying is disabled by default", {
  output <- capture_output(df_copy <- dockerfile(from = "package_markdown/units/"))
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("File copying can be disabled with NA/NA_character", {
  output <- capture_output(df_copy <- dockerfile(from = "package_markdown/units/", copy = NA_character_))
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction if NA_charachter_")

  output <- capture_output(df_copy2 <- dockerfile(from = "package_markdown/units/", copy = NA))
  expect_false(object = any(sapply(df_copy2@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction if NA")
})

test_that("File copying can be disabled with NULL", {
  output <- capture_output(df_copy <- dockerfile(from = "package_markdown/units/", copy = NULL))
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("Packaging fails if dependency is missing and predetection is disabled", {
  skip_on_cran() # CRAN knows all packages

  output <- capture_output(
    expect_warning( # Gets a warning: "generated a condition with class packageNotFoundError/error/condition. It is less fragile to test custom conditions with `class`"
      expect_error(dockerfile(from = "package_markdown/missing_dependency/", predetect = FALSE), "there is no package")
    )
  )
})

test_that("Packaging works if dependency is missing in the base image and predetection is enabled", {
  skip_on_cran() # no missing packages on on CRAN
  skip_on_ci()

  output <- capture_output({
    predetected_df <- dockerfile(from = "package_markdown/missing_dependency/",
                                 maintainer = "o2r",
                                 image = getImageForVersion("3.4.4"),
                                 predetect = TRUE)
  })
  # write(predetected_df, "package_markdown/missing_dependency/Dockerfile")

  expect_s4_class(predetected_df, "Dockerfile")
  # package should still not be in this session library
  expect_error(library("boxoffice"))

  generated_file <- unlist(stringr::str_split(toString(predetected_df),"\n"))
  expect_true(object = any(grepl("^RUN.*install2.*\"boxoffice\"", x = generated_file)), info = "Packages missing are detected")
  expect_true(object = any(grepl("^RUN.*install2.*\"rprojroot\"", x = generated_file)), info = "Packages missing are detected")

  expected_file <- readLines("package_markdown/missing_dependency/Dockerfile")
  expect_equal(capture.output(print(predetected_df)), expected_file)

  unlink("package_markdown/missing_dependency/*.md")
})
