# Copyright 2018 Opening Reproducible Research (https://o2r.info)

library("containerit")
context("expose instruction")

# EXPOSE <port> [<port>/<protocol>...]
test_that("Expose instructions can be created and added to a Dockerfile", {
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

  output <- capture_output(the_dockerfile <- dockerfile(clean_session()))
  addInstruction(the_dockerfile) <- list(expose1, expose2c)
  df_str <- toString(the_dockerfile)
  expect_true(toString(expose1) %in% df_str)
  expect_true(toString(expose2c) %in% df_str)
})
