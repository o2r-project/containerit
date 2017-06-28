# Copyright 2017 Opening Reproducible Research (http://o2r.info)

library(containerit)
context("install_github")

test_that("github_packages can be installed", {
  skip_if_not_installed("harbor")
  skip_if_not_installed("sysreqs")

  #get session information from previous installation
  load("./github/.RData")

  df = dockerfile(github_test_sessionInfo, maintainer = "matthiashinz", r_version = "3.3.2")
  #write(df, "./github/Dockerfile")

  expected_file <- readLines("./github/Dockerfile")
  generated_file <- unlist(stringr::str_split(toString(df),"\n"))
  expect_equal(generated_file, expected_file)
})

test_that("GitHub references can be retrieved (using package harbor)", {
  skip_if_not_installed("harbor")
  ref <- getGitHubRef("harbor")
  expect_match(ref, "wch/harbor@([a-f0-9]{7})")
})

test_that("GitHub references can be retrieved (using package sysreqs)", {
  skip_if_not_installed("sysreqs")
  ref <- getGitHubRef("sysreqs")
  expect_match(ref, "r-hub/sysreqs@([a-f0-9]{7})")
})
