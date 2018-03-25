# Copyright 2017 Opening Reproducible Research (http://o2r.info)

context("find system requrirements")

test_that("system requirements for selected CRAN packages can be determinded OFFLINE", {
  deps <- .find_system_dependencies("sp",
                                    platform = .debian_platform,
                                    soft = TRUE,
                                    offline = TRUE)
  deps_expected <-
    c("libproj-dev", "libgdal-dev", "gdal-bin", "libgeos-dev")
  expect_true(all(deps %in% deps_expected))
  expect_true(all(deps_expected %in% deps))

  deps <- containerit:::.find_system_dependencies("sp",
                                                  platform = containerit:::.debian_platform,
                                                  soft = FALSE,
                                                  offline = TRUE)
  expect_equal(deps, character(0)) #no direct dependencies

  deps <- .find_system_dependencies("rgdal",
                                    platform = .debian_platform,
                                    soft = TRUE,
                                    offline = TRUE)
  deps_expected <- c("libgdal-dev", "gdal-bin", "libproj-dev")
  expect_true(all(deps %in% deps_expected))
  expect_true(all(deps_expected %in% deps))

  deps <- .find_system_dependencies("rgdal",
                                    platform = .debian_platform,
                                    offline = TRUE)
  deps_expected <- c("libgdal-dev", "gdal-bin", "libproj-dev")
  expect_true(all(deps %in% deps_expected))
  expect_true(all(deps_expected %in% deps))
})

test_that("system requirements for CRAN packages can be determinded ONLINE", {
  deps <- .find_system_dependencies("sp", platform = .debian_platform, soft = FALSE)
  expect_equal(deps, character(0)) #no direct dependencies

  deps <- .find_system_dependencies("rgdal", platform = .debian_platform)
  deps_expected <- c("libgdal-dev", "gdal-bin", "libproj-dev")
  expect_true(all(deps %in% deps_expected))
  expect_true(all(deps_expected %in% deps))

})
