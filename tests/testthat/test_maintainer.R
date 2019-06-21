# Copyright 2018 Opening Reproducible Research (https://o2r.info)

library(containerit)
context("maintainer instruction")

test_that("a Maintainer instruction can be created", {
  expect_warning(obj <- Maintainer(name = "Matthias Hinz", email = "matthias.m.hinz@gmail.com"),
                 "deprecated")
  instruction <- toString(obj)
  expect_equal(instruction,
               "MAINTAINER \"Matthias Hinz\" matthias.m.hinz@gmail.com")
  expect_equal(instruction, as.character(obj))
})
