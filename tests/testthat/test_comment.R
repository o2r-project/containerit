# Copyright 2018 Opening Reproducible Research (https://o2r.info)

library("containerit")
context("Adding comments to Dockerfile")

test_that("Can create comments objects with text", {
  cmt1 <- Comment(text = "the text")
  str1 <- toString(cmt1)
  expect_equal(str1, "# the text")
})

test_that("Can add comments to a Dockerfile", {
  output <- capture_output(the_dockerfile <- dockerfile(clean_session()))
  addInstruction(the_dockerfile) <- Comment(text = "the text")
  addInstruction(the_dockerfile) <- Label(foo = "bar")
  addInstruction(the_dockerfile) <- Comment(text = "after foo bar")
  df_str <- toString(the_dockerfile)
  expect_equal(df_str[[4]], "# the text")
  expect_equal(df_str[[6]], "# after foo bar")
})
