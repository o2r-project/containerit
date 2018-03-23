# Copyright 2017 Opening Reproducible Research (http://o2r.info)

context("Packaging R-Scripts and workspaces.")

test_that("the R script location is checked ",{
  expect_error(dockerfile("falseScriptLocation.R"))
})

test_that("an R script can be created with resources of the same folder ",{
  df <- dockerfile("script_resources/simple_test.R",
                copy = "script_dir",
                cmd = CMD_Rscript("script_resources/simple_test.R"),
                maintainer = "matthiashinz",
                image = getImageForVersion("3.3.2"))
  #for overwriting
  #write(df, "script_resources/Dockerfile")

  # test run (shoud be fast and not give any errors)
  image <- create_localDockerImage(df, use_workdir = TRUE)
  harbor::docker_run(image = image, rm = TRUE)
  harbor::docker_cmd(harbor::localhost, "rmi", image)

  expected_file <- readLines("script_resources/Dockerfile")
  generated_file <- unlist(stringr::str_split(toString(df),"\n"))
  expect_equal(generated_file, expected_file)
})

test_that("a workspace with one R script can be packaged ",{
  #This test should result in the same dockerfile as above:
  df <- dockerfile("script_resources/",
                copy = "script_dir",
                cmd = CMD_Rscript("script_resources/simple_test.R"),
                maintainer = "matthiashinz",
                image = getImageForVersion("3.3.2"))

  expected_file <- readLines("script_resources/Dockerfile")
  expect_equal(toString(df), expected_file)
})

test_that("a list of resources can be packaged ",{
  df <- dockerfile("script_resources/simple_test.R",
                copy = c("script_resources/simple_test.R",
                         "script_resources/test_table.csv",
                         "script_resources/test_subfolder/testresource"),
                maintainer = "matthiashinz",
                image = getImageForVersion("3.3.2"))
  #for overwriting
  #write(df, "script_resources/Dockerfile2")
  expected_file <- readLines("script_resources/Dockerfile2")
  generated_file <- unlist(stringr::str_split(toString(df),"\n"))
  expect_equal(generated_file, expected_file)
})

test_that("there is an error if non-existing resources are to be packages",{
  expect_error(dockerfile("script_resources/simple_test.R",
                          copy = c("does_not_exist.R")))
})

test_that("The gstat demo 'zonal' can be packaged ",{
  skip_if_not_installed("sp")
  skip_if_not_installed("gstat")

  df <- dockerfile("script_gstat/zonal.R",
                cmd = CMD_Rscript("script_gstat/zonal.R"),
                maintainer = "matthiashinz",
                image = getImageForVersion("3.3.2"),
                copy = "script")

  #for overwriting
  #write(df, "script_gstat/Dockerfile")
  #test execution would be similar to the test above; will not be done because of slowlieness
  expected_file = readLines("script_gstat/Dockerfile")
  generated_file <- unlist(stringr::str_split(toString(df),"\n"))
  expect_equal(generated_file, expected_file)
})

test_that("The file can be copied", {
  df_copy <- dockerfile(from = "script_resources/simple_test.R", copy = "script")
  expect_true(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })),
              info = "at least one Copy instruction")
})

test_that("File copying is disabled by default", {
  df_copy <- dockerfile(from = "script_resources/simple_test.R")
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("File copying is disabled with NA", {
  df_copy <- dockerfile(from = "script_resources/simple_test.R", copy = NA)
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("File copying is disabled with NA_character", {
  df_copy <- dockerfile(from = "script_resources/simple_test.R", copy = NA_character_)
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("File copying is disabled with NULL", {
  df_copy <- dockerfile(from = "script_resources/simple_test.R", copy = NULL)
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("the installation order of packages is alphabetical (= reproducible)", {
  df <- dockerfile("script_packages/", maintainer = "o2r",
                   image = getImageForVersion("3.4.3", nearest = FALSE),
                   copy = "script")
  expected_file = readLines("script_packages/Dockerfile")
  expect_equal(toString(df), expected_file)
})
