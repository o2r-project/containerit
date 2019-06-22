# Copyright 2018 Opening Reproducible Research (https://o2r.info)

context("Packaging with explicit versioning")

test_that("generation function for single package works", {
  instruction <- containerit:::versioned_install_instruction(name = "fortunes", version = "1.0-2")
  expect_s4_class(instruction,"Instruction")

  instr_string <- utils::capture.output(print(instruction))
  expect_equal(instr_string, "RUN [\"Rscript\", \"-e\", \"versions::install.versions('fortunes', '1.0-2')\"]")
})

test_that("error during build when installing too old a version", {
  skip_if_not(stevedore::docker_available())

  instruction <- containerit:::versioned_install_instruction(name = "fortunes", version = "1.0-2")
  instruction_string <- toString(instruction)
  the_dockerfile_dir <- tempdir()
  writeLines(c("FROM rocker/geospatial:3.4.3", "RUN install2.r versions", instruction_string),
             con = file.path(the_dockerfile_dir, "Dockerfile"))

  client <- stevedore::docker_client()
  output <- capture_output(expect_error(client$image$build(context = the_dockerfile_dir, dockerfile = "Dockerfile")))
})

test_that("generation function for multiple packages works", {
  pkgs <- data.frame(name = c("fortunes", "sf"), version = c("1.2-1", "0.2-0"), source = c("CRAN", "CRAN"))

  instruction <- containerit:::versioned_install_instructions(pkgs)
  expect_s4_class(instruction,"Instruction")

  instr_string <- toString(instruction)
  expect_equal(instr_string, "RUN [\"Rscript\", \"-e\", \"versions::install.versions('fortunes', '1.2-1')\", \"-e\", \"versions::install.versions('sf', '0.2-0')\"]")
})

test_info <- list()
test_info$R.version <- list()
test_info$R.version$major <- "3"
test_info$R.version$minor <- "4.3"

test_info$otherPkgs <- list()
test_info$otherPkgs$testpkg1 <- list()
test_info$otherPkgs$testpkg1$Package <- "cowsay"
test_info$otherPkgs$testpkg1$Repository <- "CRAN"
test_info$otherPkgs$testpkg1$Version <- "0.5.0"
test_info$otherPkgs$testpkg2$Package <- "fortunes"
test_info$otherPkgs$testpkg2$Repository <- "CRAN"
test_info$otherPkgs$testpkg2$Version <- "1.5-3"

class(test_info) <- "sessionInfo"

test_that("dockerfile with versions has only one unversioned install2.r for version package, and otherwise only versioned install statements", {
  output <- capture_output(the_dockerfile <- dockerfile(from = test_info,
                                                        maintainer = "o2r",
                                                        versioned_packages = TRUE))
  expect_s4_class(the_dockerfile,"Dockerfile")

  expected_file <- readLines("package_versions/Dockerfile")
  generated_file <- unlist(stringr::str_split(toString(the_dockerfile),"\n"))
  expect_equal(generated_file, expected_file)
})

test_that("versions install commands are sorted by package name", {
  pkgs <- data.frame(name = c("a", "f", "c", "b"), version = c("1", "2", "3", "4"))

  instruction <- containerit:::versioned_install_instructions(pkgs)

  params <- instruction@params # uneven params are '-e'
  expect_equal(params[2], "versions::install.versions('a', '1')")
  expect_equal(params[4], "versions::install.versions('b', '4')")
  expect_equal(params[6], "versions::install.versions('c', '3')")
  expect_equal(params[8], "versions::install.versions('f', '2')")
})

test_that("generated versioned Dockerfile can be build and executed", {
  skip_if_not(stevedore::docker_available())

  output <- capture_output(the_dockerfile <- dockerfile(from = test_info,
                                                        maintainer = "o2r",
                                                        versioned_packages = TRUE))

  the_dockerfile_dir <- tempdir()
  write(x = the_dockerfile, file = file.path(the_dockerfile_dir, "Dockerfile"))

  output <- capture_output({
    client <- stevedore::docker_client()
    build <- client$image$build(context = the_dockerfile_dir, dockerfile = "Dockerfile", tag = "containerit_test_versioned_packages")
    run <- client$container$run(image = build$id(), rm = TRUE, cmd = c('Rscript',
                                                                       '-e', 'library(\"fortunes\");',
                                                                       '-e', 'library(\"cowsay\");',
                                                                       '-e', 'sessionInfo();'))
  })
  expect_match(toString(run$logs), "cowsay_0.5.0")
  expect_match(toString(run$logs), "fortunes_1.5-3")
})
