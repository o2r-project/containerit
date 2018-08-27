# Copyright 2018 Opening Reproducible Research (https://o2r.info)

context("Packaging R-Scripts and workspaces.")

test_that("the R script location is checked ",{
  expect_error(dockerfile("falseScriptLocation.R"))
})

test_that("an R script can be created with resources of the same folder ",{
  the_dockerfile <- dockerfile("package_script/resources/simple_test.R",
                copy = "script_dir",
                cmd = CMD_Rscript("package_script/resources/simple_test.R"),
                maintainer = "o2r",
                image = getImageForVersion("3.3.2"))
  #write(the_dockerfile,"package_script/resources/Dockerfile")

  image <- docker_build(x = the_dockerfile, use_workdir = TRUE)

  client <- stevedore::docker_client()
  runOutput <- client$container$run(image = image, rm = TRUE)

  expect_false(is.null(runOutput))
  expect_match(toString(runOutput$logs), "R version 3.3.2")
  expect_match(toString(runOutput$logs), "Hello from containerit!")

  client$image$remove(name = image, force = TRUE)

  expected_file <- readLines("package_script/resources/Dockerfile")
  generated_file <- unlist(stringr::str_split(toString(the_dockerfile),"\n"))
  expect_equal(generated_file, expected_file)
})

test_that("a workspace with one R script can be packaged",{
  the_dockerfile <- dockerfile("package_script/resources/",
                copy = "script_dir",
                cmd = CMD_Rscript("package_script/resources/simple_test.R"),
                maintainer = "o2r",
                image = getImageForVersion("3.3.2"))

  expected_file <- readLines("package_script/resources/Dockerfile")
  expect_equal(toString(the_dockerfile), expected_file)
})

test_that("a workspace with one R script can be packaged if the script file has .r (lowercase) extension",{
  #This test should result in the same dockerfile as above:
  the_dockerfile <- dockerfile("package_script/simple_lowercase/",
                               copy = "script_dir",
                               cmd = CMD_Rscript("package_script/simple_lowercase/simple_test.r"),
                               maintainer = "o2r",
                               image = getImageForVersion("3.3.2"))

  expected_file <- readLines("package_script/simple_lowercase/Dockerfile")
  expect_equal(toString(the_dockerfile), expected_file)
})

test_that("a list of resources can be packaged ",{
  the_dockerfile <- dockerfile("package_script/resources/simple_test.R",
                copy = c("package_script/resources/simple_test.R",
                         "package_script/resources/test_table.csv",
                         "package_script/resources/test_subfolder/testresource"),
                maintainer = "o2r",
                image = getImageForVersion("3.3.2"))
  #for overwriting
  #write(the_dockerfile,"package_script/resources/Dockerfile2")
  expected_file <- readLines("package_script/resources/Dockerfile2")
  generated_file <- unlist(stringr::str_split(toString(the_dockerfile),"\n"))
  expect_equal(generated_file, expected_file)
})

test_that("there is an error if non-existing resources are to be packages",{
  expect_error(dockerfile("package_script/resources/simple_test.R",
                          copy = c("does_not_exist.R")))
})

test_that("The gstat demo 'zonal' can be packaged ",{
  skip_if_not_installed("sp")
  skip_if_not_installed("gstat")

  the_dockerfile <- dockerfile("package_script/gstat/zonal.R",
                cmd = CMD_Rscript("package_script/gstat/zonal.R"),
                maintainer = "o2r",
                image = getImageForVersion("3.3.2"),
                copy = "script")

  #for overwriting
  #write(the_dockerfile,"package_script/gstat/Dockerfile")
  #test execution would be similar to the test above; will not be done because of slowlieness
  expected_file = readLines("package_script/gstat/Dockerfile")
  generated_file <- unlist(stringr::str_split(toString(the_dockerfile),"\n"))
  expect_equal(generated_file, expected_file)
})

test_that("The file can be copied", {
  df_copy <- dockerfile(from = "package_script/resources/simple_test.R", copy = "script")
  expect_true(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })),
              info = "at least one Copy instruction")
})

test_that("File copying is disabled by default", {
  df_copy <- dockerfile(from = "package_script/resources/simple_test.R")
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("File copying is disabled with NA", {
  df_copy <- dockerfile(from = "package_script/resources/simple_test.R", copy = NA)
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("File copying is disabled with NA_character", {
  df_copy <- dockerfile(from = "package_script/resources/simple_test.R", copy = NA_character_)
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("File copying is disabled with NULL", {
  df_copy <- dockerfile(from = "package_script/resources/simple_test.R", copy = NULL)
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("the installation order of packages is alphabetical (= reproducible)", {
  the_dockerfile <- dockerfile("package_script/resources/", maintainer = "o2r",
                   image = getImageForVersion("3.3.2", nearest = FALSE),
                   copy = "script",
                   cmd = CMD_Rscript("package_script/resources/simple_test.R"))
  expected_file = readLines("package_script/resources/Dockerfile3")
  expect_equal(toString(the_dockerfile), expected_file)
})

test_that("packaging works if library from script is missing but predetection is enabled", {
  skip_on_cran() # CRAN knows all the packages

  # install package to new library path
  test_lib_path <- tempfile("test_lib_")
  dir.create(test_lib_path)
  generated_file <- callr::r_vanilla(function() {
    library("containerit")
    the_dockerfile <- dockerfile(from = "package_script/needs_predetect/", maintainer = "o2r", predetect = TRUE)
    generated_file <- unlist(stringr::str_split(toString(the_dockerfile),"\n"))
    generated_file
  }, libpath = c(test_lib_path, .libPaths()), repos = "http://cloud.r-project.org")
  expect_equal(list.files(test_lib_path), c("boxoffice"))

  expected_file <- readLines("package_script/needs_predetect/Dockerfile")
  expect_equal(generated_file, expected_file)
  expect_true(object = any(grepl("^RUN.*install2.*\"boxoffice\"", x = generated_file)), info = "Packages missing are detected")

  unlink(test_lib_path)
})
