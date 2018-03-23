# Copyright 2017 Opening Reproducible Research (http://o2r.info)

library(containerit)
context("comment instruction")

test_that("can create comments",{
  cmt1 <- Comment(text = "the text")
  str1 <- toString(cmt1)
  expect_equal(str1, "# the text")
})

test_that("can add comments to a Dockerfile",{
  df <- dockerfile(clean_session())
  addInstruction(df) <- cmt1
  addInstruction(df) <- Label(foo = "bar")
  addInstruction(df) <- Comment(text = "after foo bar")
  df_str <- toString(df)
  expect_equal(df_str[[4]], "# the text")
  expect_equal(df_str[[6]], "# after foo bar")
})
