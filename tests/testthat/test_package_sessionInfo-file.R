# Copyright 2017 Opening Reproducible Research (http://o2r.info)

context("Packaging sessionInfo saved in .Rdata file")

# create the test data, also a good way to understand what information from session info is actually used!
rdata_file <- tempfile(pattern = "containerit_", tmpdir = tempdir(), fileext = ".Rdata")

sessionInfo <- list()
sessionInfo$R.version <- list()
sessionInfo$R.version$major <- "1"
sessionInfo$R.version$minor <- "2.3"

sessionInfo$basePkgs <- c("stats", "graphics", "grDevices", "utils", "datasets", "methods", "base")

sessionInfo$otherPkgs <- list()
sessionInfo$otherPkgs$testpkg1 <- list()
sessionInfo$otherPkgs$testpkg1$Version <- "1.0"
sessionInfo$otherPkgs$testpkg1$Package <- "testpkg1"
sessionInfo$otherPkgs$testpkg1$RemoteType <- "github"
sessionInfo$otherPkgs$testpkg1$RemoteRepo <- "pkg1"
sessionInfo$otherPkgs$testpkg1$RemoteUsername <- "test"
sessionInfo$otherPkgs$testpkg1$RemoteSha <- "123456abcdef"
sessionInfo$otherPkgs$testpkg2 <- list()
sessionInfo$otherPkgs$testpkg2$Version <- "1.2.3"
sessionInfo$otherPkgs$testpkg2$Package <- "testpkg2"
sessionInfo$otherPkgs$testpkg2$Repository <- "CRAN"
sessionInfo$otherPkgs$testpkg3$Version <- "1.0"
sessionInfo$otherPkgs$testpkg3$Package <- "testpkg3"
sessionInfo$otherPkgs$testpkg3$RemoteType <- "github"
sessionInfo$otherPkgs$testpkg3$GithubRepo <- "pkg3"
sessionInfo$otherPkgs$testpkg3$GithubUsername <- "test"
sessionInfo$otherPkgs$testpkg3$GithubSHA1 <- "a1b2c3d4e5f6"

sessionInfo$loadedOnly <- list()
sessionInfo$otherPkgs$loadedA <- list()
sessionInfo$otherPkgs$loadedA$Version <- "0.1.2"
sessionInfo$otherPkgs$loadedA$Package <- "loadedA"
sessionInfo$otherPkgs$loadedA$Repository <- "CRAN"
sessionInfo$otherPkgs$loadedB <- list()
sessionInfo$otherPkgs$loadedB$Version <- "1.0-42"
sessionInfo$otherPkgs$loadedB$Package <- "loadedB"
sessionInfo$otherPkgs$loadedB$Repository <- "CRAN"

class(sessionInfo) <- "sessionInfo"
save(sessionInfo, file = rdata_file)

test_that("can create dockerfile object from the file with sessionInfo", {
  df <- dockerfile(from = rdata_file)
  expect_s4_class(df, "Dockerfile")
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
  # FIXME not implmented yet
})

test_that("GitHub reference can be retrieved from sessionInfo", {
  ref <- getGitHubRef("testpkg1", c(sessionInfo$otherPkgs, sessionInfo$loadedOnly))
  expect_equal(ref, "test/pkg1@123456abcdef")
})

test_that("R version can be retrieved from sessionInfo", {
  ver <- getRVersionTag(sessionInfo)
  expect_equal(ver, "1.2.3")
})

test_that("error if Rdata file contains more than one object", {
  file <- tempfile(tmpdir = tempdir(), fileext = ".Rdata")
  a <- "1"
  b <- "2"
  save(a, sessionInfo, b, file = file)
  expect_error(getSessionInfoFromRdata(file), "only one")
})

test_that("Matching image can be retrieved from sessionInfo, with a warning", {
  expect_warning(getImageForVersion(getRVersionTag(sessionInfo)), "No Docker image found")
  image <- getImageForVersion(getRVersionTag(sessionInfo))
  expect_equal(toString(image), "FROM rocker/r-ver:3.1.0")
})

test_that("Version tag can be retrieved from sessionInfo.Rdata file, with a warning", {
  ver <- getRVersionTag(rdata_file)
  expect_equal(ver, "1.2.3")
})
