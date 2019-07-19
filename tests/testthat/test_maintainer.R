# Copyright 2018 Opening Reproducible Research (https://o2r.info)

library("containerit")
context("MAINTAINER instruction")

test_that("a MAINTAINER instruction can be created", {
  expect_warning(obj <- Maintainer(name = "Matthias Hinz", email = "matthias.m.hinz@gmail.com"),
                 "deprecated")
  instruction <- toString(obj)
  expect_equal(instruction,
               "MAINTAINER \"Matthias Hinz\" matthias.m.hinz@gmail.com")
  expect_equal(instruction, as.character(obj))
})

test_that("Users can specify the maintainer", {
  maintainer <- methods::new("Maintainer", name = "Matthias Hinz", email = "matthias.m.hinz@gmail.com")
  output <- capture_output(dfile <- dockerfile(NULL, maintainer = maintainer))

  expect_is(slot(dfile, "maintainer"), "Maintainer")
  mslot = methods::slot(dfile, "maintainer")
  expect_equal(attr(class(mslot), "package"), "containerit")
  expect_equal(slot(mslot, "name"), "Matthias Hinz")
  expect_equal(slot(mslot, "email"), "matthias.m.hinz@gmail.com")
  expect_equal(toString(mslot),
               "MAINTAINER \"Matthias Hinz\" matthias.m.hinz@gmail.com")
})

test_that("The default of maintainer is the current system user, and the default is a label-maintainer", {
  output <- capture_output(dfile <- dockerfile())
  expect_is(slot(dfile, "maintainer"), "Label")
  mslot = methods::slot(dfile, "maintainer")
  expect_equal(slot(mslot, "data")[["maintainer"]], Sys.info()[["user"]])
  expect_equal(toString(mslot), paste0("LABEL maintainer=\"", Sys.info()[["user"]], "\""))
})
