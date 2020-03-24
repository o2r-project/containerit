# Copyright 2018 Opening Reproducible Research (https://o2r.info)

library("containerit")
context("Install packages from GitHub")

the_session <- clean_session(expr = quote(library("sysreqs")))

test_that("GitHub packages can be installed", {
  output <- capture_output(
    the_dockerfile <- dockerfile(the_session, maintainer = "o2r", image = getImageForVersion("3.3.2"))
  )
  #write(the_dockerfile,"./github/Dockerfile")

  expected_file <- readLines("./github/Dockerfile")
  generated_file <- unlist(stringr::str_split(toString(the_dockerfile),"\n"))
  expect_equal(generated_file, expected_file)
})

test_that("GitHub references can be retrieved for package sysreqs (test fails if sysreqs was installed from source)", {
  skip_if_not_installed("sysreqs")
  ref <- getGitHubRef("sysreqs", the_session$otherPkgs)
  expect_match(ref, "r-hub/sysreqs@([a-f0-9]{7})")
})

test_that("the package remotes is installed if not already in the list of packages", {
  output <- capture_output(
    the_dockerfile <- dockerfile(from = "github/DESCRIPTION",
                                 maintainer = "o2r")
  )
  expect_true(any(stringr::str_detect(toString(the_dockerfile),
                                      "^RUN \\[\"install2.r\", \"containerittest\", \"graphics\", \"remotes\"\\]$")))
  expect_true(any(stringr::str_detect(toString(the_dockerfile),
                                      "^RUN \\[\"installGithub.r\", \"some-org/the_package@master\"\\]$")))
})

test_that("the package remotes is installed in the correct version if not already in the list of packages", {
  output <- capture_output(
    the_dockerfile <- dockerfile(from = "github/DESCRIPTION",
                                 maintainer = "o2r",
                                 versioned_packages = TRUE)
  )
  expect_true(any(stringr::str_detect(toString(the_dockerfile),
                                      "versions::install.versions\\('remotes', '1.1.1'\\)")))
})
