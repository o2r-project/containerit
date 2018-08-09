# Copyright 2018 Opening Reproducible Research (https://o2r.info)

context("linter")

test_that("package style is good", {
  skip_if_not_installed("lintr")
  skip("do not lint")

  requireNamespace("lintr", quietly = TRUE)
  lintr::expect_lint_free()
})
