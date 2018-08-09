# Copyright 2018 Opening Reproducible Research (https://o2r.info)

library(containerit)
context("entrypoint instruction")

test_that("Error when nothing is provided to Entrypoint constructor", {
  expect_error(Entrypoint())
})

test_that("Error when non-Entrypoint object is provided to Dockerfile", {
  expect_error(dockerfile(from = NULL, entrypoint = "Rscript -e '1+1'"))
})

test_that("Error when NULL is provided as program", {
  expect_error(Entrypoint(program = NULL))
})

test_that("Error when not a list is provided as params", {
  expect_error(Entrypoint(program = "top", params = "a b"))
})

test_that("Entrypoint program is correctly rendered", {
  ep <- Entrypoint("echo")
  expect_equal(as.character(ep), 'ENTRYPOINT ["echo"]')
})

test_that("Entrypoint program with path is correctly rendered in exec form", {
  ep <- Entrypoint("/bin/echo")
  expect_equal(as.character(ep), 'ENTRYPOINT ["/bin/echo"]')
})

test_that("Entrypoint program with path is correctly rendered in shell form", {
  ep <- Entrypoint("/bin/echo", form = "shell")
  expect_equal(as.character(ep), 'ENTRYPOINT /bin/echo')
})

test_that("Unsupported form gives error", {
  expect_error(Entrypoint("echo", form = "unsupp"), "unsupp")
})

test_that("Entrypoint parameters are correctly rendered in exec form", {
  ep <- Entrypoint(program = "Rscript", params = list("sum.R", "1", "2"))
  expect_equal(as.character(ep), 'ENTRYPOINT ["Rscript", "sum.R", "1", "2"]')

  ep <- Entrypoint(program = "Rscript", params = list("sum.R", "1", "2"), form = "exec")
  expect_equal(as.character(ep), 'ENTRYPOINT ["Rscript", "sum.R", "1", "2"]')
})

test_that("Entrypoint parameters correctly rendered in shell form", {
  ep <- Entrypoint(program = "Rscript", params = list("sum.R", "1", "2"), form = "shell")
  expect_equal(as.character(ep), 'ENTRYPOINT Rscript sum.R 1 2')
})

test_that("Entrypoint command is correctly added to Dockerfile as second to last line", {
  the_dockerfile <- dockerfile(from = NULL,
                   entrypoint = Entrypoint("Rscript"),
                   cmd = Cmd("script.R"))
  df_string <- toString(the_dockerfile)
  expect_equal(df_string[length(df_string) - 1], 'ENTRYPOINT ["Rscript"]')
})

test_that("Entrypoint command is correctly rendered to file", {
  the_dockerfile <- dockerfile(from = NULL, image = "ubuntu",
                   maintainer = NULL, container_workdir = NULL,
                   entrypoint = Entrypoint("top", list("-b")),
                   cmd = Cmd(params = c("-c")))
  tempfile <- file.path(tempdir(), "Dockerfile.entrypoint")
  write(the_dockerfile, file = tempfile)

  control_file <- "./entrypoint/Dockerfile"
  control_instructions <- readLines(control_file)
  generated_instructions <- readLines(tempfile)
  #compare generated file with permanent file
  expect_equal(control_instructions, generated_instructions)

  unlink(tempfile)
})
