# Copyright 2017 Opening Reproducible Research (http://o2r.info)

library(testthat)
library(containeRit)
context("Package R markdown files")

test_that("A simple Sweave file can be packaged", {
  expect_false(file.exists("knitr-minimal.tex"))
  expect_false(file.exists("knitr-minimal.pdf"))
  
  sweave <- system.file("examples", "knitr-minimal.Rnw", package = "knitr")
  temp_sweave = "package_markdown/knitr-minimal.Rnw"
  expect_false(file.exists(temp_sweave))
  #copy file intu build context:
  expect_true(file.copy(sweave, temp_sweave))
  
  df <- dockerfile(temp_sweave, 
                   copy = "script",
                   maintainer = Maintainer("matthiashinz"),
                   r_version = "3.3.2")
  
  #write(df, "package_markdown/knitr_minimal_Dockerfile")
  expected_file <- readLines("package_markdown/knitr_minimal_Dockerfile")
  generated_file <- unlist(stringr::str_split(format(df),"\n"))
  expect_equal(generated_file, expected_file)
  
  expect_true(file.exists("knitr-minimal.tex"))
  unlink("knitr-minimal.tex")
  
  expect_true(file.exists("knitr-minimal.pdf"))
  unlink("knitr-minimal.pdf")
  unlink(temp_sweave)
  unlink("figure", recursive = TRUE)
})




test_that("A markdown file can be packaged (using markdowntainer-units-expample)", {
  expected_output = c("package_markdown/markdowntainer-units/2016-09-29-plot_units.html", "package_markdown/markdowntainer-units/images")
  expect_false(all(file.exists(expected_output)))
  
  #let containerIt find the markdownfile by itself
  df <- dockerfile("package_markdown/markdowntainer-units/",  
                   maintainer = Maintainer("matthiashinz"),
                   r_version = "3.3.2",
                   copy = "script_dir",
                   cmd = CMD_Render("package_markdown/markdowntainer-units/2016-09-29-plot_units.Rmd"))
  #for overwriting:
  #write(df,"package_markdown/units_Dockerfile")
  expected_file <- readLines("package_markdown/units_Dockerfile")
  generated_file <- unlist(stringr::str_split(format(df),"\n"))
  expect_equal(generated_file, expected_file)
  
  expect_true(all(file.exists(expected_output)))
  unlink(expected_output,recursive = TRUE)
})




test_that("A sf markdown file can be packaged", {
  expect_false(file.exists("package_markdown/sf"))
  md_file <- system.file("doc/sf3.Rmd",package = "sf")
  expect_false(file.exists("package_markdown/sf"))
  dir.create("package_markdown/sf")
  expect_true(file.copy(md_file, "package_markdown/sf/"))
  #let containerIt find the markdownfile by itself
  df <- dockerfile("package_markdown/sf",
                   maintainer = Maintainer("matthiashinz"),
                   image = "rocker/geospatial",
                   copy = "script_dir",
                   cmd = CMD_Render("package_markdown/sf/", output_dir = "/output"))
  #for overwriting:
  #write(df,"package_markdown/sf_vignette_Dockerfile")
  expected_file = readLines("package_markdown/sf_vignette_Dockerfile")
  generated_file <- unlist(stringr::str_split(format(df),"\n"))
  expect_equal(generated_file, expected_file)

  expect_true(file.exists("package_markdown/sf/sf3.html"))
  unlink("package_markdown/sf",recursive = TRUE)
})

