# Copyright 2016 Opening Reproducible Research (http://o2r.info)

library(testthat)
library(containeRit)

#for testing manually:
if(FALSE){
  #assuming that workdir is the containerit-folder:
  library(futile.logger)
  setwd("tests/testthat")
  source("../../R/sessionInfo-localbuild-methods.R")
  source("../../R/package-installation-methods.R")
  source("../../R/defaults.R")
  #(run tests)
  setwd("../../")
}

#R CMD check fails for some test if not setting the environment variable
# See https://github.com/hadley/testthat/issues/144
Sys.setenv("R_TESTS" = "")

test_check("containeRit")