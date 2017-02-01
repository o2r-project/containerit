# Copyright 2016 Opening Reproducible Research (http://o2r.info)

library(testthat)
library(containerit)



#R CMD check fails for some test if not setting the environment variable
# See https://github.com/hadley/testthat/issues/144
Sys.setenv("R_TESTS" = "")

test_check("containerit")