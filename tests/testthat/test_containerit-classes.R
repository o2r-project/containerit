# Copyright 2016 Opening Reproducible Research (http://o2r.info)

library(containerit)
context("class-testing")

test_that("a Maintainer instruction can be created", {
  obj <-
    new("Maintainer", name = "Matthias Hinz", email = "matthias.m.hinz@gmail.com")
  instruction <- toString(obj)
  expect_equal(instruction,
               "MAINTAINER \"Matthias Hinz\" matthias.m.hinz@gmail.com")
  expect_equal(instruction, as.character(obj))
})

test_that("A From instruction can be created", {
  obj <- new("From", image = "myimage")
  instruction <- toString(obj)
  expect_equal(instruction, "FROM myimage")
  expect_equal(instruction, as.character(obj))
  
  instruction <- toString(new(
    "From",
    image = "myimage",
    postfix = new("Digest", "digest")
  ))
  expect_equal(instruction, "FROM myimage@digest")
  
  instruction <- toString(new(
    "From",
    image = "myimage",
    postfix = new("Tag", "mytag")
  ))
  expect_equal(instruction, "FROM myimage:mytag")
})


test_that("A valid Cmd instruction can be created" , {
  obj <- Cmd("R")
  expect_equal(toString(obj), "CMD [\"R\"]")
  #  
  #with parameters
  obj <- Cmd("R", params = c("--vanilla", "--no-restore"))
  expect_equal(toString(obj),
               "CMD [\"R\", \"--vanilla\", \"--no-restore\"]")
  #
  #only with parameters (default parameters for entrypoint)
  obj <- Cmd(params = c("--vanilla", "--no-restore"))
  expect_equal(toString(obj), "CMD [\"--vanilla\", \"--no-restore\"]")
  #
  #invalid objects cannot be created:
  expect_error(Cmd())
  expect_error(Cmd(exec = ""))
  expect_error(Cmd(exec = NA_character_))
  expect_error(Cmd(params = c("", NA_character_)))
  expect_error(Cmd(params = c("", "param")))
})


test_that("A valid Run instruction can be created" , {
  obj <- Run("R")
  expect_equal(toString(obj), "RUN [\"R\"]")
  #
  #with parameters
  obj <- Run("Rscript", params = c("-e 1+1"))
  expect_equal(toString(obj), "RUN [\"Rscript\", \"-e 1+1\"]")
  #
  #invalid objects cannot be created:
  expect_error(Run())
  expect_error(Run(exec = ""))
  expect_error(Run(exec = NA_character_))
  expect_error(Run(exec = "R", params = c("param", NA_character_)))
  expect_error(Run(exec = "R", params = c("param", "")))
})
