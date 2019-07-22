# Copyright 2018 Opening Reproducible Research (https://o2r.info)

context("Find system requrirements")

test_that("System requirements for sp can be detected OFFLINE and soft", {
  output <- capture_output(
    deps <- containerit:::.find_system_dependencies("sp",
                                      platform = containerit:::.debian_platform,
                                      soft = TRUE,
                                      offline = TRUE)
    )
  deps_expected <- c("libproj-dev", "libgdal-dev", "gdal-bin", "libgeos-dev libgeos++-dev")
  expect_true(all(deps %in% deps_expected))
  expect_true(all(deps_expected %in% deps))
})

test_that("System requirements for sp can be detected OFFLINE and unsoft", {
  output <- capture_output(
    deps <- containerit:::.find_system_dependencies("sp",
                                                    platform = containerit:::.debian_platform,
                                                    soft = FALSE,
                                                    offline = TRUE)
    )
  expect_equal(deps, character(0)) #no direct dependencies
})

test_that("System requirements for rgdal can be detected OFFLINE", {
  output <- capture_output(
    deps <- containerit:::.find_system_dependencies("rgdal",
                                                    platform = containerit:::.debian_platform,
                                                    soft = TRUE,
                                                    offline = TRUE)
  )
  deps_expected <- c("libgdal-dev", "gdal-bin", "libproj-dev")
  expect_true(all(deps %in% deps_expected))
  expect_true(all(deps_expected %in% deps))

  output <- capture_output(
    deps <- containerit:::.find_system_dependencies("rgdal",
                                                    platform = containerit:::.debian_platform,
                                                    offline = TRUE)
  )
  deps_expected <- c("libgdal-dev", "gdal-bin", "libproj-dev")
  expect_true(all(deps %in% deps_expected))
  expect_true(all(deps_expected %in% deps))
})

test_that("System requirements for sp can be detected ONLINE", {
  output <- capture_output(
    deps <- containerit:::.find_system_dependencies("sp",
                                                    platform = containerit:::.debian_platform,
                                                    soft = FALSE)
  )
  expect_equal(deps, character(0)) #no direct dependencies
})

test_that("System requirements for rgdal can be detected ONLINE", {
  output <- capture_output(
    deps <- containerit:::.find_system_dependencies("rgdal",
                                                    platform = containerit:::.debian_platform)
  )
  deps_expected <- c("libgdal-dev", "gdal-bin", "libproj-dev")
  expect_true(all(deps %in% deps_expected))
  expect_true(all(deps_expected %in% deps))
})
