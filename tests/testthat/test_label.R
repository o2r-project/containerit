# Copyright 2018 Opening Reproducible Research (https://o2r.info)

library(containerit)
context("label instruction")

test_that("Labels of arbitrary content can be added to a Dockerfile",{
  label1 <- Label(key1 = "this", key2 = "that", otherKey = "content")
  str1 <- toString(label1)

  expect_equal(str1, "LABEL key1=\"this\" key2=\"that\" otherKey=\"content\"")

  label2 <- Label(key1 = "this", key2 = "that", othekey = "content", multi_line = TRUE)
  str2 <- toString(label2)
  expect_equal(str2, "LABEL key1=\"this\" \\\n\tkey2=\"that\" \\\n\tothekey=\"content\"")

  df <- dockerfile(clean_session())
  addInstruction(df) <- list(label1, label2)
  df_str <- toString(df)
  expect_true(str1 %in% df_str)
  expect_true(str2 %in% df_str)
})
