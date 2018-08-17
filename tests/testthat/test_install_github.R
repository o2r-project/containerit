# Copyright 2018 Opening Reproducible Research (https://o2r.info)

library(containerit)
context("installation of packages from GitHub")

test_that("GitHub packages can be installed", {
  #get session information from previous installation, created in a vanilla R session with these commands within package root directory:
  #library(c("sysreqs")); github_test_sessionInfo <- sessionInfo(); save(github_test_sessionInfo, file = "tests/testthat/github/sessionInfo.RData")
  load("./github/sessionInfo.RData")

  the_dockerfile <- dockerfile(github_test_sessionInfo, maintainer = "o2r", image = getImageForVersion("3.3.2"))
  #write(the_dockerfile,"./github/Dockerfile")

  expected_file <- readLines("./github/Dockerfile")
  generated_file <- unlist(stringr::str_split(toString(the_dockerfile),"\n"))
  expect_equal(generated_file, expected_file)
})

test_that("GitHub references can be retrieved for package sysreqs (test fails if sysreqs was installed from source)", {
  skip_if_not_installed("sysreqs")
  ref <- getGitHubRef("sysreqs", c(sessionInfo()$otherPkgs, sessionInfo()$loadedOnly))
  expect_match(ref, "r-hub/sysreqs@([a-f0-9]{7})")
})
