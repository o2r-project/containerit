# Copyright 2018 Opening Reproducible Research (https://o2r.info)

context("Packaging sessionInfo saved in .Rdata file")

# create the test data, also a good way to understand what information from session info is actually used!
rdata_file <- tempfile(pattern = "containerit_", tmpdir = tempdir(), fileext = ".Rdata")

test_info <- list()
test_info$R.version <- list()
test_info$R.version$major <- "1"
test_info$R.version$minor <- "2.3"

test_info$basePkgs <- c("stats", "graphics", "grDevices", "utils", "datasets", "methods", "base")

test_info$otherPkgs <- list()
test_info$otherPkgs$testpkg1 <- list()
test_info$otherPkgs$testpkg1$Version <- "1.0"
test_info$otherPkgs$testpkg1$Package <- "testpkg1"
test_info$otherPkgs$testpkg1$RemoteType <- "github"
test_info$otherPkgs$testpkg1$RemoteRepo <- "pkg1"
test_info$otherPkgs$testpkg1$RemoteUsername <- "test"
test_info$otherPkgs$testpkg1$RemoteSha <- "123456abcdef"
test_info$otherPkgs$testpkg2 <- list()
test_info$otherPkgs$testpkg2$Version <- "1.2.3"
test_info$otherPkgs$testpkg2$Package <- "testpkg2"
test_info$otherPkgs$testpkg2$Repository <- "CRAN"
test_info$otherPkgs$testpkg3$Version <- "1.0"
test_info$otherPkgs$testpkg3$Package <- "testpkg3"
test_info$otherPkgs$testpkg3$RemoteType <- "github"
test_info$otherPkgs$testpkg3$GithubRepo <- "pkg3"
test_info$otherPkgs$testpkg3$GithubUsername <- "test"
test_info$otherPkgs$testpkg3$GithubSHA1 <- "a1b2c3d4e5f6"

test_info$loadedOnly <- list()
test_info$loadedOnly$loadedA <- list()
test_info$loadedOnly$loadedA$Version <- "0.1.2"
test_info$loadedOnly$loadedA$Package <- "loadedA"
test_info$loadedOnly$loadedA$Repository <- "CRAN"
test_info$loadedOnly$loadedB <- list()
test_info$loadedOnly$loadedB$Version <- "1.0-42"
test_info$loadedOnly$loadedB$Package <- "loadedB"
test_info$loadedOnly$loadedB$Repository <- "CRAN"

class(test_info) <- "sessionInfo"
sessionInfo = test_info
save(sessionInfo, file = rdata_file)

test_that("can create dockerfile object from the file with sessionInfo", {
  the_dockerfile <- dockerfile(from = rdata_file)
  expect_s4_class(the_dockerfile,"Dockerfile")
})

df_test <- dockerfile(from = rdata_file)

test_that("dockerfile object contains expected R version", {
  expect_equal(toString(df_test)[1], "FROM rocker/r-ver:3.1.0")
})

test_that("dockerfile contains CRAN packages", {
  expect_true(any(stringr::str_detect(toString(df_test),
                                      "^RUN \\[\"install2.r\", \"loadedA\", \"loadedB\", \"remotes\", \"testpkg2\"\\]$")))
})

test_that("dockerfile contains GitHub packages", {
  expect_true(any(stringr::str_detect(toString(df_test),
                                      "^RUN \\[\"installGithub.r\", \"test/pkg1@123456abcdef\", \"test/pkg3@a1b2c3d4e5f6\"\\]$")))
})

test_that("dockerfile contains package versions", {
  skip("not implemented yet")
  # FIXME not implmented yet
})

test_that("GitHub reference can be retrieved from sessionInfo", {
  ref <- getGitHubRef("testpkg1", c(test_info$otherPkgs, test_info$loadedOnly))
  expect_equal(ref, "test/pkg1@123456abcdef")
})

test_that("R version can be retrieved from sessionInfo", {
  ver <- getRVersionTag(test_info)
  expect_equal(ver, "1.2.3")
})

test_that("R version can be retrieved from session_info", {
  info <- list()
  info$platform <- list(version = "R version 7.8.9 (2063-04-05)")
  class(info) <- c("session_info")
  ver <- getRVersionTag(info)
  expect_equal(ver, "7.8.9")

  ver2 <- getRVersionTag(sessioninfo::session_info())
  expect_equal(ver2, paste0(R.version$major, ".", R.version$minor), "version extraction from sessioninfo::session_info")

  ver3 <- getRVersionTag(devtools::session_info())
  expect_equal(ver3, paste0(R.version$major, ".", R.version$minor), "version extraction from devtools::session_info")
})

test_that("error if Rdata file contains more than one object", {
  file <- tempfile(tmpdir = tempdir(), fileext = ".Rdata")
  a <- "1"
  b <- "2"
  save(a, test_info, b, file = file)
  expect_error(getSessionInfoFromRdata(file), "exactly one")
})

test_that("Matching image can be retrieved from sessionInfo, with a warning", {
  expect_warning(getImageForVersion(getRVersionTag(test_info)), "No Docker image found")
  image <- getImageForVersion(getRVersionTag(test_info))
  expect_equal(toString(image), "FROM rocker/r-ver:3.1.0")
})

test_that("Version tag can be retrieved from sessionInfo.Rdata file, with a warning", {
  ver <- getRVersionTag(rdata_file)
  expect_equal(ver, "1.2.3")
})

test_that("Object name 'sessioninfo' is accepted.", {
  rdata_file <- tempfile(pattern = "containerit_", tmpdir = tempdir(), fileext = ".Rdata")
  sessioninfo <- test_info
  save(sessioninfo, file = rdata_file)
  the_dockerfile <- dockerfile(from = rdata_file)
  expect_s4_class(the_dockerfile,"Dockerfile")
})

test_that("Object name 'session_info' is accepted.", {
  rdata_file <- tempfile(pattern = "containerit_", tmpdir = tempdir(), fileext = ".Rdata")
  session_info <- test_info
  save(session_info, file = rdata_file)
  the_dockerfile <- dockerfile(from = rdata_file)
  expect_s4_class(the_dockerfile,"Dockerfile")
})

test_that("Version tag can be retrieved from .Rdata file with session_info", {
  rdata_file <- tempfile(pattern = "containerit_", tmpdir = tempdir(), fileext = ".Rdata")
  session_info <- sessioninfo::session_info()
  save(session_info, file = rdata_file)
  ver <- getRVersionTag(rdata_file)
  expect_equal(ver, paste0(R.version$major, ".", R.version$minor))

  session_info <- devtools::session_info()
  save(session_info, file = rdata_file)
  ver2 <- getRVersionTag(rdata_file)
  expect_equal(ver2, paste0(R.version$major, ".", R.version$minor))
})

test_that("sessioninfo::session_info() is supported", {
  rdata_file <- tempfile(pattern = "containerit_", tmpdir = tempdir(), fileext = ".Rdata")
  session_info <- sessioninfo::session_info()
  save(session_info, file = rdata_file)
  the_dockerfile <- dockerfile(from = rdata_file)
  expect_s4_class(the_dockerfile,"Dockerfile")
})

test_that("devtools::session_info() is supported", {
  rdata_file <- tempfile(pattern = "containerit_", tmpdir = tempdir(), fileext = ".Rdata")
  session_info <- devtools::session_info()
  save(session_info, file = rdata_file)
  the_dockerfile <- dockerfile(from = rdata_file)
  expect_s4_class(the_dockerfile,"Dockerfile")
})
