test_that("system requirements for CRAN packages can be determinded", {
  #test may have to be adjusted if system requriements change
  deps <- .find_system_dependencies("sp", method = "sysreq-package")
  deps_expected <-
    c("libproj-dev", "libgdal-dev", "gdal-bin", "libgeos-dev")
  expect_true(all(deps %in% deps_expected))
  expect_true(all(deps_expected %in% deps))
  
  deps <-
    .find_system_dependencies("rgdal", method = "sysreq-package")
  deps_expected <-
    c("libgdal-dev",
      "gdal-bin",
      "libproj-dev",
      "libgdal-dev",
      "gdal-bin")
  expect_true(all(deps %in% deps_expected))
  expect_true(all(deps_expected %in% deps))
  
})