# Copyright 2016 Opening Reproducible Research (http://o2r.info)

context("linter")

test_that("Package Style", {
  skip_if_not_installed("lintr")

  requireNamespace("lintr", quietly = TRUE)
  lintr::expect_lint_free()
})
