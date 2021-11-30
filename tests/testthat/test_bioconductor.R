# Copyright 2018 Opening Reproducible Research (https://o2r.info)

library("containerit")
context("Install packages from Bioconductor")

test_that("installation instruction for Bioconductor package is created", {
  skip_if(Sys.getenv("R_VERSION") == "devel")

  output <- capture_output({
    capture_warnings({
      if (!require("BiocGenerics", character.only = TRUE)) BiocManager::install(c("BiocGenerics"))
    })
    info <- containerit::clean_session(expr = quote(library("BiocGenerics")))
    the_dockerfile <- dockerfile(info, maintainer = "o2r", image = getImageForVersion("3.3.2"))
  })
  #write(the_dockerfile,"./bioconductor/Dockerfile")

  expected_file <- readLines("./bioconductor/Dockerfile")
  generated_file <- unlist(stringr::str_split(toString(the_dockerfile),"\n"))
  testthat::skip_if_not_installed("BiocVersion")
  current_bioc_version =  as.character(packageVersion("BiocVersion")[,1:2])
  #!!! Unsure as to whether the R image 3.3.2 is supposed to
  # know to force the BiocVersion to that required for R 3.3.2,
  # not the current version

  # expected_file = stringr::str_replace_all(
  #   expected_file,
  #   "packages/3.9",
  #   paste0("packages/", current_bioc_version))
  expect_equal(generated_file, expected_file)
})
