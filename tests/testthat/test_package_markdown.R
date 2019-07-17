# Copyright 2018 Opening Reproducible Research (https://o2r.info)

context("Package R markdown files")

test_that("A markdown file can be packaged (using units expample)", {
  output <- capture_output(
    the_dockerfile <- dockerfile(from = "package_markdown/units/",
                   maintainer = "Ted Tester",
                   image = getImageForVersion("3.3.2"),
                   copy = "script_dir",
                   cmd = CMD_Render("package_markdown/units/2016-09-29-plot_units.Rmd"))
  )
  #write(the_dockerfile,"package_markdown/units_Dockerfile")
  expected_file <- readLines("package_markdown/units_Dockerfile")
  generated_file <- unlist(stringr::str_split(toString(the_dockerfile),"\n"))
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
  skip_on_cran() # cannot remove packages on CRAN
  skip_on_ci()

  expect_warning(if (require("abe")) remove.packages("abe"))

  expect_error(library("abe"))

  # install package to new library path
  test_lib_path <- tempfile("test_lib_")
  dir.create(test_lib_path)
  output <- capture_output({
    the_dockerfile <- callr::r_vanilla(function() {
      library("containerit")
      the_dockerfile <- dockerfile(from = "package_markdown/missing_dependency/", maintainer = "o2r", predetect = TRUE)
      the_dockerfile
    },
    libpath = c(test_lib_path, .libPaths()), repos = "https://cloud.r-project.org")
  })

  expect_s4_class(the_dockerfile, "Dockerfile")
  expect_equal(list.files(test_lib_path), c("abe", "boxoffice"))
  expect_error(library("abe"))

  generated_file <- unlist(stringr::str_split(toString(the_dockerfile),"\n"))
  expect_true(object = any(grepl("^RUN.*install2.*\"boxoffice\"", x = generated_file)), info = "Packages missing are detected")
  expect_true(object = any(grepl("^RUN.*install2.*\"abe\"", x = generated_file)), info = "Packages missing are detected")

  unlink(test_lib_path)
  unlink("package_markdown/missing_dependency/*.md")
})
