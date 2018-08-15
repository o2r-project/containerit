# Copyright 2018 Opening Reproducible Research (https://o2r.info)

library(containerit)
context("comment instruction")

test_that("can create comments",{
  cmt1 <- Comment(text = "the text")
  str1 <- toString(cmt1)
  expect_equal(str1, "# the text")
})

test_that("can add comments to a Dockerfile", {
  the_dockerfile <- dockerfile(clean_session())
  addInstruction(the_dockerfile) <- Comment(text = "the text")
  addInstruction(the_dockerfile) <- Label(foo = "bar")
  addInstruction(the_dockerfile) <- Comment(text = "after foo bar")
  df_str <- toString(the_dockerfile)
  expect_equal(df_str[[4]], "# the text")
  expect_equal(df_str[[6]], "# after foo bar")
})
