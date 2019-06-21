# Copyright 2018 Opening Reproducible Research (https://o2r.info)

library(containerit)
context("Metadata Creation: Label Schema factory")

test_that("Build-time labels can be created", {
  output <- capture_output(factory <- LabelSchemaFactory())
  bdate <- Sys.time()
  label <- factory(name = "ImageName", description = "Description of the image", build_date = bdate)
  bdate_format <- format.POSIXct(bdate, "%Y-%m-%dT%H:%M:%OS%z")
  labelstr <- toString(label)
  expected <- paste0("LABEL org.label-schema.schema-version=\"1.0.0-rc.1\" ",
                     "\\\n\torg.label-schema.build-date=\"2017-03-21T19:13:47+0100\"",
                     " \\\n\torg.label-schema.name=\"ImageName\"",
                     " \\\n\torg.label-schema.description=\"Description of the image\"")
  expected <- stringr::str_replace(expected, pattern = "2017-03-21T19:13:47\\+0100", bdate_format)
  expect_equal(labelstr, expected)
})



