# Copyright 2017 Opening Reproducible Research (http://o2r.info)

library(testthat)
library(containerit)
context("Package Sweave files")

test_that("A simple Sweave file can be packaged", {
  temp_sweave = "package_markdown/knitr-minimal.Rnw"
  unlink("knitr-minimal.tex")
  unlink("knitr-minimal.pdf")
  unlink(temp_sweave)
  unlink("figure", recursive = TRUE)

  sweave <- system.file("examples", "knitr-minimal.Rnw", package = "knitr")
  expect_true(file.copy(sweave, temp_sweave))

  df <- dockerfile(temp_sweave,
                   copy = "script",
                   maintainer = "matthiashinz",
                   r_version = "3.3.2")

  write(df, "package_markdown/knitr_minimal_Dockerfile")
  expected_file <- readLines("package_markdown/knitr_minimal_Dockerfile")
  generated_file <- unlist(stringr::str_split(toString(df),"\n"))
  expect_equal(generated_file, expected_file)

  unlink("knitr-minimal.tex")
  unlink("knitr-minimal.pdf")
  unlink(temp_sweave)
  unlink("figure", recursive = TRUE)
})

