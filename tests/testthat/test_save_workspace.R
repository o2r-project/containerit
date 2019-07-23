# Copyright 2018 Opening Reproducible Research (https://o2r.info)

context("Save workspace and R objects (save_image - argument)")

test_that("Session objects with default file name can be containerized", {
  rm(list = ls(envir = environment()), envir = environment()) # start clean
  expect_false(file.exists(".RData"), "RData file already exists in testthat folder. Remove manually and restart test.")

  test_text <- "test"
  test_vector <- c(1:10)
  test_number <- 42
  assign("test_float", 17.42)
  assign("test_list", list(1, "two", 3.00))

  output <- capture_output(the_dockerfile <- dockerfile(save_image = TRUE, envir = environment()))

  # check Dockerfile: select copy instructions that occur after workdir instructions
  inst <- methods::slot(the_dockerfile,"instructions")
  inst_types <- sapply(inst, class)
  last_wd_sel <- max(which(inst_types == "Workdir"))
  copy_sel <- which(inst_types == "Copy")
  copy_sel <- copy_sel[copy_sel > last_wd_sel]
  expect_length(copy_sel, 1)
  expect_equal(inst[[copy_sel]], Copy(".RData", ".RData"))

  # check saved file
  expect_true(file.exists(".RData"), "The expected workspace image '.RData' file was not written to working directory.")
  rm(list = ls(envir = environment()), envir = environment())

  load(".RData", envir = environment())
  expect_equal(ls(envir = environment()), c("test_float", "test_list", "test_number", "test_text", "test_vector"))
  expect_equal(test_text, "test")
  expect_equal(test_number, 42)
  expect_false("save_image_filename" %in% ls(envir = environment()))

  unlink(".RData") # clean up
})

test_that("Selected session objects with configured file name can be containerized", {
  rm(list = ls(envir = environment()), envir = environment()) # start clean
  expect_false(file.exists("test_file.RData"), "RData file already exists in testthat folder. Remove manually and restart test.")

  test_text <- "test"
  test_number <- 42
  test_list <- list(one = "two", "data" = c(1:10))

  output <- capture_output(the_dockerfile <- dockerfile(save_image = list("test_text",
                                                                          save_image_filename = "test_file.RData", "test_list"),
                                                        envir = environment()))

  # check Dockerfile: select copy instructions that occur after workdir instructions
  inst <- methods::slot(the_dockerfile,"instructions")
  inst_types <- sapply(inst, class)
  last_wd_sel <- max(which(inst_types == "Workdir"))
  copy_sel <- which(inst_types == "Copy")
  copy_sel <- copy_sel[copy_sel > last_wd_sel]
  expect_length(copy_sel, 1)
  expect_equal(inst[[copy_sel]], Copy("test_file.RData", "test_file.RData"))

  # check saved file
  expect_true(file.exists("test_file.RData"), "The expected workspace image file was not written to working directory.")
  rm(list = ls(envir = environment()), envir = environment())

  load("test_file.RData", envir = environment())
  expect_equal(ls( envir = environment()), c("test_list", "test_text"))
  expect_false("test_number" %in% ls( envir = environment()))

  unlink("test_file.RData") # clean up
})

test_that("Program ignores unsupported input for save_image", {
  output <- capture_output(
    expect_s4_class(dockerfile(from = sessionInfo(), save_image = data.frame()), "Dockerfile")
  )
  output <- capture_output(
    expect_s4_class(dockerfile(from = sessionInfo(), save_image = matrix()), "Dockerfile")
  )

  unlink(".RData")
})
