# Copyright 2018 Opening Reproducible Research (https://o2r.info)

library("containerit")
context("run instruction")

test_that("A valid Run instruction can be created" , {
  obj <- Run("R")
  expect_equal(toString(obj), "RUN [\"R\"]")
})

test_that("A valid Run instruction can be created with parameters" , {
  obj <- Run("Rscript", params = c("-e 1+1"))
  expect_equal(toString(obj), "RUN [\"Rscript\", \"-e 1+1\"]")
})

test_that("Invalid Run instruction give error: no exec" , {
    expect_error(Run())
})

test_that("Invalid Run instruction give error: empty exec" , {
  expect_error(Run(exec = NA_character_))
  expect_error(Run(exec = ""))
})

test_that("Invalid Run instruction give error: empty params" , {
  expect_error(Run(exec = "R", params = c("param", NA_character_)))
  expect_error(Run(exec = "R", params = c("param", "")))
})
