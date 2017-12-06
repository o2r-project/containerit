# Copyright 2017 Opening Reproducible Research (http://o2r.info)

library(testthat)
library(containerit)

#for testing whole files manually:
if(FALSE){
  #assuming that workdir is the containerit-folder:
  library(futile.logger)
  futile.logger::flog.threshold(DEBUG)
  #source("R/sessionInfo-localbuild-methods.R")
  #source("R/package-installation-methods.R")
  #source("R/containerit-config.R")
  #source("R/defaults.R")

  #test_package("containerit")
  result <- test_file(path = "tests/testthat/test_find_systemrequirements.R")
  result <- test_file(path = "tests/testthat/test_package_markdown.R")
  result <- test_file(path = "tests/testthat/test_package_sweave.R")
  result <- test_file(path = "tests/testthat/test_package_Rscript.R")
  result <- test_file(path = "tests/testthat/test_sessioninfo_reproduce.R")
  result <- test_file(path = "tests/testthat/test_getImageForVersion.R")
  result <- test_file(path = "tests/testthat/test_install_github.R")
  result; str(result)
}

# for manually executing code within tests:
if(FALSE) {
  setwd(file.path(getwd(), "tests", "testthat"))
}


#R CMD check fails for some test if not setting the environment variable
# See https://github.com/hadley/testthat/issues/144
Sys.setenv("R_TESTS" = "")

test_check("containerit")
