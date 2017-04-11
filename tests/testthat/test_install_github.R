# Copyright 2017 Opening Reproducible Research (http://o2r.info)

library(containerit)
context("install_github")


test_that("github_packages can be installed", {
  
  #get session information from previous installation
  load("./github-package-resources/sessioninfo_github.RData")
  
  #set github references 
  # (cannot be determined exactly determined standard-sessionInfo 
  # TODO: consider reading this information from 'containerit_info_devtools', a devtools - session_info
  unlockBinding(as.name(".githubRefs"), env = getNamespace("containerit"))
  .addGitHubRef("geojsonio","edzer/sfr@7057e42")
  .addGitHubRef("sf","ropensci/geojsonio@2e15e21")

  df = dockerfile(containerit_info, maintainer = Maintainer("matthiashinz"), r_version = "3.3.2")
  #write(df, "./github-package-resources/Dockerfile")

  .githubRefs <- new.env()
  
  expected_file <- readLines("./github-package-resources/Dockerfile")
  generated_file <- unlist(stringr::str_split(format(df),"\n"))
  expect_equal(generated_file, expected_file)

})




