# Copyright 2017 Opening Reproducible Research (http://o2r.info)

library(containerit)
context("Instructions")


test_that("a Maintainer instruction can be created", {
  obj <-
    new("Maintainer", name = "Matthias Hinz", email = "matthias.m.hinz@gmail.com")
  instruction <- toString(obj)
  expect_equal(instruction,
               "MAINTAINER \"Matthias Hinz\" matthias.m.hinz@gmail.com")
  expect_equal(instruction, as.character(obj))
})


test_that("A From instruction can be created", {
  obj <- new("From", image = "myimage")
  instruction <- toString(obj)
  expect_equal(instruction, "FROM myimage")
  expect_equal(instruction, as.character(obj))

  instruction <- toString(new(
    "From",
    image = "myimage",
    postfix = new("Digest", "digest")
  ))
  expect_equal(instruction, "FROM myimage@digest")

  instruction <- toString(new(
    "From",
    image = "myimage",
    postfix = new("Tag", "mytag")
  ))
  expect_equal(instruction, "FROM myimage:mytag")
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


test_that("A valid Run instruction can be created" , {
  obj <- Run("R")
  expect_equal(toString(obj), "RUN [\"R\"]")
  #
  #with parameters
  obj <- Run("Rscript", params = c("-e 1+1"))
  expect_equal(toString(obj), "RUN [\"Rscript\", \"-e 1+1\"]")
  #
  #invalid objects cannot be created:
  expect_error(Run())
  expect_error(Run(exec = ""))
  expect_error(Run(exec = NA_character_))
  expect_error(Run(exec = "R", params = c("param", NA_character_)))
  expect_error(Run(exec = "R", params = c("param", "")))
})


test_that("A valid Copy instruction can be created" , {
  obj <- Copy(c("script.R","exampleFolder"),"path/to/destination")
  expect_equal(toString(obj), "COPY [\"script.R\", \"exampleFolder\", \"path/to/destination\"]")

  expect_error(Copy())
  expect_error(Copy("src"))
  expect_error(Copy(c("a","b"),c("dest","b")))
})


test_that("Labels of arbitrary content can be added to a Dockerfile",{
  label1 <- Label(key1 = "this", key2 = "that", otherKey = "content")
  str1 <- toString(label1)

  expect_equal(str1, "LABEL key1=\"this\" key2=\"that\" otherKey=\"content\"")

  label2 <- Label(key1 = "this", key2 = "that", othekey = "content", multi_line = TRUE)
  str2 <- toString(label2)
  expect_equal(str2, "LABEL key1=\"this\" \\\n\tkey2=\"that\" \\\n\tothekey=\"content\"")

  df <- dockerfile(clean_session())
  addInstruction(df) <- list(label1, label2)
  df_str <- toString(df)
  expect_true(str1 %in% df_str)
  expect_true(str2 %in% df_str)
})


# EXPOSE <port> [<port>/<protocol>...]
test_that("Expose instructions can be created and added to a Dockerfile",{
  expose1 <- Expose(port = 1234)
  expect_equal(toString(expose1), "EXPOSE 1234")

  expose1b <- Expose(port = "12345")
  expect_equal(toString(expose1b), "EXPOSE 12345")

  expose2 <- Expose(port = "2345/tcp", host = "80")
  expect_equal(toString(expose2), "EXPOSE 80 2345/tcp")

  expose2b <- Expose(port = "2345/udp", host = 81)
  expect_equal(toString(expose2b), "EXPOSE 81 2345/udp")

  expose2c <- Expose(port = "6789/TCP", host = 82)
  expect_equal(toString(expose2c), "EXPOSE 82 6789/TCP")

  expose4 <- Expose(port = "8080", host = "4567")
  expect_equal(toString(expose4), "EXPOSE 4567 8080")

  expose4b <- Expose(port = 8081, host = 5678)
  expect_equal(toString(expose4b), "EXPOSE 5678 8081")

  expect_warning(Expose(port = "2345/tcp", host = "abc"), "NAs introduced by coercion")

  expect_error(Expose(host = 123), "argument \"port\" is missing")

  expect_error(Expose(port = "1/cat"), "protocol of container must be")

  df <- dockerfile(clean_session())
  addInstruction(df) <- list(expose1, expose2c)
  df_str <- toString(df)
  expect_true(toString(expose1) %in% df_str)
  expect_true(toString(expose2c) %in% df_str)
})

#toString(Copy(c("script.R","exampleFolder"),"path/to/destination"))

