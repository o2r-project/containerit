library(testthat)
library(containeRit)


test_that("A simple Sweave file can be packaged", {
  expect_false(file.exists("knitr-minimal.tex"))
  
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
  expected_file = readLines("package_markdown/knitr_minimal_Dockerfile")
  expect_equal(format(df), expected_file)
  
  expect_true(file.exists("knitr-minimal.tex"))
  unlink("knitr-minimal.tex")
  unlink(temp_sweave)
  unlink("figure")
})



test_that("A markdown file can be packaged (using the package vignette)", {
  expect_false(file.exists("basic.md"))
  
  markdown <- system.file("doc","basic.Rmd", package = "containeRit")
  temp_markdown = "package_markdown/basic.Rmd"
  expect_false(file.exists(temp_markdown))
  #copy file intu build context:
  expect_true(file.copy(markdown, temp_markdown))
  
  #let containerIt find the markdownfile by itself
  df <- dockerfile("package_markdown/",  
             maintainer = Maintainer("matthiashinz"),
             r_version = "3.3.2")
  #for overwriting:
  #write(df,"package_markdown/basic_vignette_Dockerfile")
  expected_file = readLines("package_markdown/basic_vignette_Dockerfile")
  expect_equal(format(df), expected_file)
  
  expect_true(file.exists("basic.md"))
  unlink(temp_markdown)
  unlink("basic.md")
  unlink("figure")
})