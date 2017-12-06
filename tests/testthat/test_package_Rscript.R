# Copyright 2017 Opening Reproducible Research (http://o2r.info)

context("Packaging R-Scripts and workspaces.")

test_that("the R script location is checked ",{
  expect_error(dockerfile("falseScriptLocation.R"))
})

test_that("an R script can be created with resources of the same folder ",{
  print("Script executed locally: ")
  df <- dockerfile("simple_test_script_resources/simple_test.R",
                copy = "script_dir",
                cmd = CMD_Rscript("simple_test_script_resources/simple_test.R"),
                maintainer = "matthiashinz",
                r_version = "3.3.2")
  #test run (shoud be fast:)
  image <- create_localDockerImage(df, use_workdir = TRUE)
  print("Script reproduced with Docker: ")
  harbor::docker_run(image = image, rm = TRUE)
  harbor::docker_cmd(harbor::localhost, "rmi", image)
  #for overwriting
  #write(df, "simple_test_script_resources/Dockerfile")
  expected_file <- readLines("simple_test_script_resources/Dockerfile")
  generated_file <- unlist(stringr::str_split(toString(df),"\n"))
  expect_equal(generated_file, expected_file)
})

test_that("a workspace with one R script can be packaged ",{
  #This test should result in the same dockerfile as above:
  df=dockerfile("simple_test_script_resources/",
                copy = "script_dir",
                cmd = CMD_Rscript("simple_test_script_resources/simple_test.R"),
                maintainer = "matthiashinz",
                r_version = "3.3.2")

  expected_file <- readLines("simple_test_script_resources/Dockerfile")
  expect_equal(toString(df), expected_file)
})

test_that("a list of resources can be packaged ",{
  df <- dockerfile("simple_test_script_resources/simple_test.R",
                copy = c("simple_test_script_resources/simple_test.R",
                         "simple_test_script_resources/test_table.csv",
                         "simple_test_script_resources/test_subfolder/testresource"),
                maintainer = "matthiashinz",
                r_version = "3.3.2")
  #for overwriting
  #write(df, "simple_test_script_resources/Dockerfile2")
  expected_file <- readLines("simple_test_script_resources/Dockerfile2")
  generated_file <- unlist(stringr::str_split(toString(df),"\n"))
  expect_equal(generated_file, expected_file)
})

test_that("there is an error if non-existing resources are to be packages",{
  expect_error(dockerfile("simple_test_script_resources/simple_test.R",
                          copy = c("does_not_exist.R")))
})

test_that("The gstat demo 'zonal' can be packaged ",{
  expect_true(requireNamespace("sp"))
  expect_true(requireNamespace("gstat"))

  df <- dockerfile("test_script_gstat/zonal.R",
                cmd = CMD_Rscript("test_script_gstat/zonal.R"),
                maintainer = "matthiashinz",
                r_version = "3.3.2")

  #for overwriting
  #write(df, "test_script_gstat/Dockerfile")
  #test execution would be similar to the test above; will not be done because of slowlieness
  expected_file = readLines("test_script_gstat/Dockerfile")
  generated_file <- unlist(stringr::str_split(toString(df),"\n"))
  expect_equal(generated_file, expected_file)
})

test_that("The file is automatically copied", {
  df_copy <- dockerfile(from = "simple_test_script_resources/simple_test.R")
  expect_true(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })),
              info = "at least one Copy instruction")
})

test_that("File copying can be disabled with NA", {
  df_copy <- dockerfile(from = "simple_test_script_resources/simple_test.R", copy = NA)
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("File copying can be disabled with NA_character", {
  df_copy <- dockerfile(from = "simple_test_script_resources/simple_test.R", copy = NA_character_)
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("File copying can be disabled with NULL", {
  df_copy <- dockerfile(from = "simple_test_script_resources/simple_test.R", copy = NULL)
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("the installation order of packages is alphabetical (= reproducible)", {
  df <- dockerfile("simple_test_script_packages/", maintainer = "o2r", image = getImageForVersion("3.4.2", nearest = FALSE))
  expected_file = readLines("simple_test_script_packages/Dockerfile")
  expect_equal(toString(df), expected_file)
})
