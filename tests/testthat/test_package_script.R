# Copyright 2018 Opening Reproducible Research (https://o2r.info)

context("Packaging R-Scripts and workspace directories.")

test_that("the R script location is checked ", {
  output <- capture_output(expect_error(dockerfile("falseScriptLocation.R")))
})

test_that("warning if R script location is notwithin the working directory", {
  tmpfile <- tempfile(fileext = ".R")
  output <- capture_output({
    file.copy(from = "package_script/resources/simple_test.R", to = tmpfile, overwrite = TRUE)
    expect_warning(dockerfile(from = tmpfile), "not inside the working directory")
  })
  unlink(tmpfile)
})

test_that("an R script can be created with resources of the same folder ", {
  skip_if_not(stevedore::docker_available())

  output <- capture_output({
    the_dockerfile <- dockerfile("package_script/resources/simple_test.R",
                copy = "script_dir",
                cmd = CMD_Rscript("package_script/resources/simple_test.R"),
                maintainer = "o2r",
                image = getImageForVersion("3.3.2"))
    image <- docker_build(x = the_dockerfile, use_workdir = TRUE)
  })
  #write(the_dockerfile,"package_script/resources/Dockerfile")

  client <- stevedore::docker_client()
  output <- capture_output(runOutput <- client$container$run(image = image, rm = TRUE))

  expect_false(is.null(runOutput))
  expect_match(toString(runOutput$logs), "R version 3.3.2")
  expect_match(toString(runOutput$logs), "Hello from containerit!")

  expect_match(toString(output), "Hello from containerit!")

  client$image$remove(name = image, force = TRUE)

  expected_file <- readLines("package_script/resources/Dockerfile")
  generated_file <- unlist(stringr::str_split(toString(the_dockerfile),"\n"))
  expect_equal(generated_file, expected_file)
})

test_that("a workspace with one R script can be packaged", {
  output <- capture_output(
    the_dockerfile <- dockerfile("package_script/resources/",
                copy = "script_dir",
                cmd = CMD_Rscript("package_script/resources/simple_test.R"),
                maintainer = "o2r",
                image = getImageForVersion("3.3.2"))
  )

  expected_file <- readLines("package_script/resources/Dockerfile")
  expect_equal(toString(the_dockerfile), expected_file)
})

test_that("a workspace with one R script can be packaged if the script file has .r (lowercase) extension", {
  output <- capture_output(
    the_dockerfile <- dockerfile("package_script/simple_lowercase/",
                                 copy = "script_dir",
                                 cmd = CMD_Rscript("package_script/simple_lowercase/simple_test.r"),
                                 maintainer = "o2r",
                                 image = getImageForVersion("3.3.2"))
  )
  expected_file <- readLines("package_script/simple_lowercase/Dockerfile")
  expect_equal(toString(the_dockerfile), expected_file)
})

test_that("a list of resources can be packaged", {
  output <- capture_output(
    the_dockerfile <- dockerfile("package_script/resources/simple_test.R",
                                 copy = c("package_script/resources/simple_test.R",
                                         "package_script/resources/test_table.csv",
                                         "package_script/resources/test_subfolder/testresource"),
                                 maintainer = "o2r",
                                 image = getImageForVersion("3.3.2"))
  )
  #write(the_dockerfile,"package_script/resources/Dockerfile2")
  expected_file <- readLines("package_script/resources/Dockerfile2")
  generated_file <- unlist(stringr::str_split(toString(the_dockerfile),"\n"))
  expect_equal(generated_file, expected_file)
})

test_that("there is a warning if non-existing resources are to be copied", {
  output <- capture_output(
    expect_warning(dockerfile("package_script/resources/simple_test.R",
                            copy = c("does_not_exist.R")))
  )
})

test_that("trailing slashes are added to directories if missing", {
  skip_on_appveyor() # FIXME, need to do some tests on Windows machine

  output <- capture_output(
    the_dockerfile <- dockerfile(from = "package_script/resources/simple_test.R",
                                 copy = c("package_script",
                                          "package_script/resources/",
                                          "package_script/resources/test_subfolder"),
                                 maintainer = "o2r",
                                 image = getImageForVersion("3.3.2"))
  )
  generated_file <- unlist(stringr::str_split(toString(the_dockerfile),"\n"))
  expect_equal(generated_file[4], "COPY [\"package_script/\", \"package_script/\"]")
  expect_equal(generated_file[5], "COPY [\"package_script/resources/\", \"package_script/resources/\"]")
  expect_equal(generated_file[6], "COPY [\"package_script/resources/test_subfolder/\", \"package_script/resources/test_subfolder/\"]")
})

test_that("there is a warning if NA resources are to be copied", {
  output <- capture_output(
    expect_warning(dockerfile("package_script/resources/simple_test.R",
                              copy = c(NA, "package_script/resources/simple_test.R", NA)),
                   "The file NA, given by 'copy', does not exist!")
  )
})

test_that("The gstat demo 'zonal' can be packaged ", {
  skip_on_ci()

  output <- capture_output(
    the_dockerfile <- dockerfile("package_script/gstat/zonal.R",
                                 cmd = CMD_Rscript("package_script/gstat/zonal.R"),
                                 maintainer = "o2r",
                                 image = getImageForVersion("3.3.2"),
                                 copy = "script")
  )
  #write(the_dockerfile,"package_script/gstat/Dockerfile")

  #test execution would be similar to the test above; don't do it to save time
  expected_file = readLines("package_script/gstat/Dockerfile")
  generated_file <- capture.output(print(the_dockerfile))
  expect_equal(generated_file, expected_file)
})

test_that("The file can be copied", {
  output <- capture_output(df_copy <- dockerfile(from = "package_script/resources/simple_test.R", copy = "script"))
  expect_true(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })),
              info = "at least one Copy instruction")
})

test_that("File copying is disabled by default", {
  output <- capture_output(df_copy <- dockerfile(from = "package_script/resources/simple_test.R"))
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("File copying is disabled with NA", {
  output <- capture_output(df_copy <- dockerfile(from = "package_script/resources/simple_test.R", copy = NA))
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("File copying is disabled with NA_character", {
  output <- capture_output(df_copy <- dockerfile(from = "package_script/resources/simple_test.R", copy = NA_character_))
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("File copying is disabled with NULL", {
  output <- capture_output(df_copy <- dockerfile(from = "package_script/resources/simple_test.R", copy = NULL))
  expect_false(object = any(sapply(df_copy@instructions, function(x) { inherits(x, "Copy") })), info = "no Copy instruction")
})

test_that("the installation order of packages is alphabetical (= reproducible)", {
  output <- capture_output(
    the_dockerfile <- dockerfile("package_script/resources/", maintainer = "o2r",
                   image = getImageForVersion("3.3.2", nearest = FALSE),
                   copy = "script",
                   cmd = CMD_Rscript("package_script/resources/simple_test.R"))
  )
  expected_file = readLines("package_script/resources/Dockerfile3")
  expect_equal(capture.output(print(the_dockerfile)), expected_file)
})

test_that("packaging fails if library from script is missing without predetection", {
  skip_on_cran() # CRAN knows all the packages
  skip_on_ci()

  # package should still not be in this session library
  expect_error(library("boxoffice"))

  expect_error({
    output <- capture_output({
      callr::r_vanilla(function() {
        containerit::dockerfile(from = "package_script/needs_predetect/",
                                predetect = FALSE)
        },
        libpath = .libPaths(),
        repos = "https://cloud.r-project.org")
    })
  })
})

test_that("packaging works if library from script is missing but predetection is enabled", {
  skip_on_cran() # CRAN knows all the packages
  skip_on_ci()

  output <- capture_output({
    predetected_df <- dockerfile(from = "package_script/needs_predetect/",
                               maintainer = "o2r",
                               image = getImageForVersion("3.4.4"),
                               predetect = TRUE)
  })
  # write(predetected_df, "package_script/needs_predetect/Dockerfile")

  # package should still not be in this session's library
  expect_error(library("boxoffice"))

  expect_true(object = any(grepl("^RUN.*install2.*\"boxoffice\"", x = capture.output(print(predetected_df)))))
  expected_file <- readLines("package_script/needs_predetect/Dockerfile")
  expect_equal(capture.output(print(predetected_df)), expected_file)
})
