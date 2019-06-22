# Copyright 2018 Opening Reproducible Research (https://o2r.info)

library("containerit")
context("getImageForVersion")

test_that("the same version is returned if it exists", {
  image <- getImageForVersion("3.3.3")
  expect_equal(as.character(image), "FROM rocker/r-ver:3.3.3")
})

test_that("the closest version is returned if the requested one does not exist", {
  expect_warning(image <- getImageForVersion("3.2.99"))
  expect_equal(as.character(image), "FROM rocker/r-ver:3.2.5")
})

test_that("there is an informative warning if nearest is enabled", {
  expect_warning(getImageForVersion("3.2.99", nearest = TRUE), "closest match")
})

test_that("there is an informative warning if nearest is disabled", {
  expect_warning(getImageForVersion("3.2.99", nearest = FALSE), "returning input")
})

test_that("the requested version is returned if nearest is disabled", {
  expect_warning(the_version <- getImageForVersion("1.10.100", nearest = FALSE))
  expect_s4_class(the_version, "From")
  expect_equal(toString(slot(the_version, "postfix")), "1.10.100")
})
