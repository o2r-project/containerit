# Copyright 2017 Opening Reproducible Research (http://o2r.info)

library(containerit)
context("dockerfile-generation")

test_that("a simple dockerfile object can be saved to file", {
  t_dir <- tempfile(pattern = "dir")
  dir.create(t_dir)
  #one dockerfile is generated, one is fixed for comparism
  gen_file <- paste(t_dir, "Dockerfile", sep = "/")
  maintainer <-
    new("Maintainer", name = "Matthias Hinz", email = "matthias.m.hinz@gmail.com")
  dfile <- dockerfile(from = NULL, maintainer = maintainer)
  write(dfile, file = gen_file)
  control_file <- "./dockerfile-method-resources/simple_dockerfile"
  control_instructions <- readLines(control_file)
  #update control-file to current version
  current_version <- paste(R.version$major, R.version$minor, sep = ".")
  control_instructions[1] <- stringr::str_replace(control_instructions[1],
                                                  "rocker/r-ver:\\d.\\d.\\d",
                                                  replacement = paste0("rocker/r-ver:",current_version))

  generated_instructions <- readLines(gen_file)
  #compare generated file with permanent file
  expect_equal(control_instructions, generated_instructions)
  #
  unlink(t_dir, recursive = TRUE)
})


test_that("users can specify the maintainer", {
  maintainer <-
    new("Maintainer", name = "Matthias Hinz", email = "matthias.m.hinz@gmail.com")
  dfile <- dockerfile(NULL, maintainer = maintainer)

  expect_is(slot(dfile, "maintainer"), "Maintainer")
  mslot = slot(dfile, "maintainer")
  expect_equal(attr(class(mslot), "package"), "containerit")
  expect_equal(slot(mslot, "name"), "Matthias Hinz")
  expect_equal(slot(mslot, "email"), "matthias.m.hinz@gmail.com")
  #expect Maintainer instruction
  expect_equal(toString(mslot),
               "MAINTAINER \"Matthias Hinz\" matthias.m.hinz@gmail.com")
})


test_that("the default of maintainer is the current system user, and the default is a label-maintainer", {
  dfile <- dockerfile()

  expect_is(slot(dfile, "maintainer"), "Label")
  mslot = slot(dfile, "maintainer")
  expect_equal(slot(mslot, "data")[["maintainer"]], Sys.info()[["user"]])
  expect_equal(toString(mslot), paste0("LABEL maintainer=\"", Sys.info()[["user"]], "\""))
})

test_that("users can specify the base image", {
  imagestr <- "rocker/r-ver:3.0.0"
  fromstr <- paste("FROM", imagestr)
  dfile1 <- dockerfile(from = NULL, image = imagestr)
  expect_equal(as.character(slot(dfile1, "image")), fromstr)
  #check if from - instruction is the first (may be necessary to ignore comments in later tests)
  expect_length(which(toString(dfile1) == fromstr), 1)
  #expect that custom image is preferred over R version argument
  dfile2 <-
    dockerfile(from = NULL,
               image = imagestr,
               r_version = "3.1.0")
  expect_equal(as.character(slot(dfile2, "image")), fromstr)
  #expect_equal(toString(slot(dfile2, "image")), fromstr)
  expect_length(which(toString(dfile2) == fromstr), 1)
})

test_that("users can specify the R version", {
  versionstr <- "3.1.0"
  dfile <- dockerfile(from = NULL, r_version = versionstr)
  #check content of image and instructions slots
  expect_equal(toString(slot(slot(dfile, "image"), "postfix")), versionstr)
  expect_match(toString(dfile), versionstr, all = FALSE)
  #expect am warning if the user specifies an unsupported R version
  expect_warning(dockerfile(from = NULL, r_version = "2.0"))
})


test_that("R version is the current version if not specified otherwise", {
  dfile <- dockerfile(NULL)
  #expect that image string contains the current R version
  expect_equal(as.character(slot(slot(dfile, "image"), "postfix")),
               paste(R.Version()$major, R.Version()$minor, sep = "."))
})


test_that("The package containerit is not packaged by default", {
  info = obtain_localSessionInfo(expr = quote(library(containerit)))
  df = dockerfile(info)
  expect_false(any(stringr::str_detect(format(df), "^RUN.*containerit")))
})


test_that("The package containerit is not packaged (add_self = FALSE)", {
  info = obtain_localSessionInfo(expr = quote(library(containerit)))
  df = dockerfile(info, add_self = FALSE)
  expect_false(any(stringr::str_detect(format(df), "^RUN.*containerit")))
})


test_that("The package containerit can be packaged (add_self = TRUE)", {
  skip("containerit not yet available from CRAN")
  info = obtain_localSessionInfo(expr = quote(library(containerit)))
  df = dockerfile(info, add_self = TRUE)
  # expect_true(any(stringr::str_detect(format(df), "^RUN.*containerit")))
})



