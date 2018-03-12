# Copyright 2017 Opening Reproducible Research (http://o2r.info)

library(containerit)
context("from instruction")

test_that("A From instruction can be created", {
  obj <- methods::new("From", image = "myimage")
  instruction <- toString(obj)
  expect_equal(instruction, "FROM myimage")
  expect_equal(instruction, as.character(obj))

  instruction <- toString(new(
    "From",
    image = "myimage",
    postfix = methods::new("Digest", "digest")
  ))
  expect_equal(instruction, "FROM myimage@digest")

  instruction <- toString(new(
    "From",
    image = "myimage",
    postfix = methods::new("Tag", "mytag")
  ))
  expect_equal(instruction, "FROM myimage:mytag")
})
