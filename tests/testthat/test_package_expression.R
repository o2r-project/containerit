# Copyright 2018 Opening Reproducible Research (https://o2r.info)

context("Packaging expression")

test_that("can create a Dockerfile from expression", {
  output <- capture_output(the_dockerfile <- dockerfile(from = expression(library(sp))))
  expect_s4_class(the_dockerfile,"Dockerfile")
  expect_true(any(stringr::str_detect(toString(the_dockerfile),
                                      "^RUN \\[\"install2.r\", \"sp\"\\]$")))
})

test_that("can create a Dockerfile from expression with multiple statements", {
  output <- capture_output(the_dockerfile <- dockerfile(from = expression({library(sp);library(fortunes)})))
  expect_s4_class(the_dockerfile,"Dockerfile")
  expect_true(any(stringr::str_detect(toString(the_dockerfile),
                                      "^RUN \\[\"install2.r\", \"fortunes\", \"sp\"\\]$")))
})

test_that("can create a Dockerfile from expression vector", {
  output <- capture_output(the_dockerfile <- dockerfile(from = c(expression(library(sp)), expression(library(rprojroot)))))
  expect_s4_class(the_dockerfile,"Dockerfile")
  expect_true(any(stringr::str_detect(toString(the_dockerfile),
                                      "^RUN \\[\"install2.r\", \"rprojroot\", \"sp\"\\]$")))
})
