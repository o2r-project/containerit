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
  expect_equal(generated_file, expected_file)
})
