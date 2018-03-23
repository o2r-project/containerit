# Copyright 2017 Opening Reproducible Research (http://o2r.info)

library(containerit)
context("install_github")

test_that("github_packages can be installed", {
  skip_if_not_installed("harbor")
  skip_if_not_installed("sysreqs")

  #get session information from previous installation, created with these commands and moved to the desired location afterwards:
  #library(c("harbor", "sysreqs")); github_test_sessionInfo <- sessionInfo(); save(github_test_sessionInfo, file = "sessionInfo.Rdata")
  load("./github/sessionInfo.Rdata")

  df = dockerfile(github_test_sessionInfo, maintainer = "matthiashinz", image = getImageForVersion("3.3.2"))
  write(df, "./github/Dockerfile")

  expected_file <- readLines("./github/Dockerfile")
  generated_file <- unlist(stringr::str_split(toString(df),"\n"))
  expect_equal(generated_file, expected_file)
})

test_that("GitHub references can be retrieved for package sysreqs (test fails if package was installed from source)", {
  skip_if_not_installed("sysreqs")
  ref <- getGitHubRef("sysreqs", c(sessionInfo()$otherPkgs, sessionInfo()$loadedOnly))
  expect_match(ref, "r-hub/sysreqs@([a-f0-9]{7})")
})
