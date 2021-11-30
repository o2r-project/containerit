# Copyright 2018 Opening Reproducible Research (https://o2r.info)

context("Included Dockerfiles")

test_that("minimal Dockerfile can be built and run", {
  skip_if_not(stevedore::docker_available())

  client <- stevedore::docker_client()
  context_dir = system.file("docker/minimal", package = "containerit")
  if (file.exists(context_dir)) {
    output <- capture_output({
      # image <- client$image$build(context = "../../inst/docker/minimal", tag = "cntnrt-min")
      image <- client$image$build(context = context_dir, tag = "cntnrt-min")
      res <- client$container$run(image = "cntnrt-min",
                                  cmd = c("R", "-e", "library(containerit); print(dockerfile())"),
                                  rm = TRUE)
    })
    on.exit({
      image$remove()
    })

    expect_match(toString(res$logs), "R is free software")
    expect_match(toString(res$logs), "Trying to determine system requirements")
    expect_match(toString(res$logs), "FROM rocker/r-ver")
  }
})
