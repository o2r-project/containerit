# Copyright 2016 Opening Reproducible Research (http://o2r.info)

#rename this file to test_lintr.R in order to activate test

if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Package Style", {
    lintr::expect_lint_free()
  })
}