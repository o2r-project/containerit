# Copyright 2017 Opening Rcmdroducible Research (http://o2r.info)

library(containerit)
context("cmd instruction")

test_that("Error when nothing is provided to Cmd constructor", {
  expect_error(Cmd())
})

test_that("Error when non-Entrypoint object is provided to Dockerfile", {
  expect_error(dockerfile(from = NULL, cmd = "script.R"))
})

test_that("Cmd executable is correctly rendered", {
  cmd <- Cmd("echo")
  expect_equal(as.character(cmd), 'CMD ["echo"]')
})

test_that("Cmd with path is correctly rendered in exec form", {
  cmd <- Cmd("/bin/echo")
  expect_equal(as.character(cmd), 'CMD ["/bin/echo"]')
})

test_that("Cmd with path is correctly rendered in shell form", {
  cmd <- Cmd("/bin/echo", form = "shell")
  expect_equal(as.character(cmd), 'CMD /bin/echo')
})

test_that("Unsupported form gives error", {
  expect_error(Cmd("echo", form = "unsupp"), "unsupp")
})

test_that("Cmd parameters are correctly rendered in exec form", {
  cmd <- Cmd(exec = "script.R", params = c("1", "2"), form = "exec")
  expect_equal(as.character(cmd), 'CMD ["script.R", "1", "2"]')
})

test_that("Cmd parameters are correctly rendered in shell form", {
  cmd <- Cmd(exec = "script.R", params = c("1", "2"), form = "shell")
  expect_equal(as.character(cmd), 'CMD script.R 1 2')
})

test_that("Cmd command is correctly added to Dockerfile as last line", {
  df <- dockerfile(from = NULL,
                   entrypoint = Entrypoint("Rscript"),
                   cmd = Cmd("script.R"))
  df_string <- toString(df)
  expect_equal(df_string[length(df_string)], 'CMD ["script.R"]')
})

test_that("A valid Cmd instruction can be created" , {
  obj <- Cmd("R")
  expect_equal(toString(obj), "CMD [\"R\"]")
  #
  #with parameters
  obj <- Cmd("R", params = c("--vanilla", "--no-restore"))
  expect_equal(toString(obj),
               "CMD [\"R\", \"--vanilla\", \"--no-restore\"]")
  #
  #only with parameters (default parameters for entrypoint)
  obj <- Cmd(params = c("--vanilla", "--no-restore"))
  expect_equal(toString(obj), "CMD [\"--vanilla\", \"--no-restore\"]")
  #
  #invalid objects cannot be created:
  expect_error(Cmd())
  expect_error(Cmd(exec = ""))
  expect_error(Cmd(exec = NA_character_))
  expect_error(Cmd(params = c("", NA_character_)))
  expect_error(Cmd(params = c("", "param")))
})
