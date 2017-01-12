# Copyright 2016 Opening Reproducible Research (http://o2r.info)

library(containerit)
context("class-testing")

test_that("a Maintainer instruction can be created",{
  obj = new("Maintainer", name="Matthias Hinz", email="matthias.m.hinz@gmail.com")
  instruction = toString(obj)
  expect_equal(instruction, "MAINTAINER \"Matthias Hinz\" matthias.m.hinz@gmail.com")
  expect_equal(instruction, as.character(obj))
})

test_that("A From instruction can be created",{
  obj = new("From", image="myimage")
  instruction = toString(obj)
  expect_equal(instruction, "FROM myimage")
  expect_equal(instruction, as.character(obj))
  
  instruction = toString(new("From", image="myimage", postfix=new("Digest","digest")))
  expect_equal(instruction, "FROM myimage@digest")
  
  instruction = toString(new("From", image="myimage", postfix=new("Tag","mytag")))
  expect_equal(instruction, "FROM myimage:mytag")
})



