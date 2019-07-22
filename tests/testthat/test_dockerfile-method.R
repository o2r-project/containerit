# Copyright 2018 Opening Reproducible Research (https://o2r.info)

context("Dockerfile generation")

test_that("Dockerfile object can be saved to file", {
  t_dir <- tempfile(pattern = "dir")
  dir.create(t_dir)

  gen_file <- paste(t_dir, "Dockerfile", sep = "/")
  output <- capture_output({
    dfile <- dockerfile(from = NULL,
                        maintainer = NULL,
                        image = getImageForVersion("3.4.1"))
    write(dfile, file = gen_file)
    })

  control_instructions <- readLines("Dockerfile.savetest")
  generated_instructions <- readLines(gen_file)
  expect_equal(control_instructions, generated_instructions)

  unlink(t_dir, recursive = TRUE)
})

test_that("Users can specify the base image", {
  imagestr <- "rocker/r-ver:3.0.0"
  fromstr <- paste("FROM", imagestr)
  output <- capture_output(dfile1 <- dockerfile(from = NULL, image = imagestr))
  expect_equal(as.character(slot(dfile1, "image")), fromstr)
  #check if from - instruction is the first (may be necessary to ignore comments in later tests)
  expect_length(which(toString(dfile1) == fromstr), 1)
})

test_that("Users can specify the R version", {
  versionstr <- "3.1.0"
  output <- capture_output(dfile <- dockerfile(from = NULL, image = getImageForVersion(versionstr)))
  #check content of image and instructions slots
  expect_equal(toString(slot(slot(dfile, "image"), "postfix")), versionstr)
  expect_match(toString(dfile), versionstr, all = FALSE)
})

test_that("Users are warned if an unsupported R version is set", {
  output <- capture_output({
    expect_warning(dockerfile(from = NULL, image = getImageForVersion("2.0.0")), "returning closest match")
  })
})

test_that("The R version is the current version if not specified otherwise", {
  output <- capture_output(dfile <- dockerfile(NULL))
  #expect that image string contains the current R version
  expect_equal(as.character(slot(slot(dfile, "image"), "postfix")),
               paste(R.Version()$major, R.Version()$minor, sep = "."))
})

test_that("The package containerit is not packaged by default", {
  skip_on_cran()
  skip_on_ci()

  output <- capture_output({
    info <- clean_session(expr = quote(library("containerit")))
    the_dockerfile <- dockerfile(info)
    })
  expect_false(any(stringr::str_detect(format(the_dockerfile), "^RUN.*containerit")))
})

test_that("The package containerit is not packaged (add_self = FALSE)", {
  output <- capture_output({
    info <- clean_session(expr = quote(library("containerit")))
    the_dockerfile <- dockerfile(info, add_self = FALSE)
  })
  expect_false(any(stringr::str_detect(format(the_dockerfile), "^RUN.*containerit")))
})

test_that("The package containerit can be packaged (add_self = TRUE)", {
  skip("containerit not yet available from CRAN")
  output <- capture_output({
    info <- clean_session(expr = quote(library("containerit")))
    the_dockerfile <- dockerfile(info, add_self = TRUE)
  })
  expect_true(any(stringr::str_detect(format(the_dockerfile), "^RUN.*containerit")))
})
