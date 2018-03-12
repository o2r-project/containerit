# Copyright 2017 Opening Reproducible Research (http://o2r.info)

library(containerit)
context("maintainer instruction")

test_that("a Maintainer instruction can be created", {
  obj <-
    methods::new("Maintainer", name = "Matthias Hinz", email = "matthias.m.hinz@gmail.com")
  instruction <- toString(obj)
  expect_equal(instruction,
               "MAINTAINER \"Matthias Hinz\" matthias.m.hinz@gmail.com")
  expect_equal(instruction, as.character(obj))
})
