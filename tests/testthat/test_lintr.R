# Copyright 2016 Opening Reproducible Research (http://o2r.info)

context("linter")

test_that("package style is good", {
  skip_if_not_installed("lintr")
  skip("do not lint")

  requireNamespace("lintr", quietly = TRUE)
  lintr::expect_lint_free()
})
