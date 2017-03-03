# Copyright 2017 Opening Reproducible Research (http://o2r.info)

context("find system requrirements")

test_that("system requirements for CRAN packages can be determinded", {
  #test may have to be adjusted if system requriements change
  deps <- .find_system_dependencies("sp", method = "sysreq-package", platform = .debian_platform, soft = TRUE)
  deps_expected <-
    c("libproj-dev", "libgdal-dev", "gdal-bin", "libgeos-dev")
  expect_true(all(deps %in% deps_expected))
  expect_true(all(deps_expected %in% deps))
  
  deps <- .find_system_dependencies("sp", method = "sysreq-package", platform = .debian_platform, soft = FALSE)
  expect_equal(deps, character(0)) #no direct dependencies
  
  deps <- .find_system_dependencies("rgdal", method = "sysreq-package", platform = .debian_platform, soft = TRUE)
  deps_expected <- c("libgdal-dev", "gdal-bin","libproj-dev")
  expect_true(all(deps %in% deps_expected))
  expect_true(all(deps_expected %in% deps))
  
  deps <- .find_system_dependencies("rgdal", method = "sysreq-api", platform = .debian_platform)
  deps_expected <- c("libgdal-dev", "gdal-bin","libproj-dev")
  expect_true(all(deps %in% deps_expected))
  expect_true(all(deps_expected %in% deps))
  
})
