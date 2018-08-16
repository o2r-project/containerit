# Copyright 2018 Opening Reproducible Research (https://o2r.info)

library("testthat")
library("containerit")

# for testing whole files manually:
if (FALSE){
  # fix a cache path for stevedore, see
  options(stevedore.spec.path = getwd())

  #assuming that workdir is the containerit-folder:
  library("futile.logger")
  futile.logger::flog.threshold(futile.logger::DEBUG)
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

# for manually executing code within the tests directory, use the following wd to resolve the relative paths:
if (FALSE) {
  setwd(file.path(getwd(), "tests", "testthat"))
}


#R CMD check fails for some test if not setting the environment variable
# See https://github.com/hadley/testthat/issues/144
Sys.setenv("R_TESTS" = "")

test_check("containerit")
