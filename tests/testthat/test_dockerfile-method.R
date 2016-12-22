# Copyright 2016 Opening Reproducible Research (http://o2r.info)

library(containerit)
context("dockerfile-generation")

test_that("A basic dockerfile with maintainer can be generated",{
  t_dir=tempfile(pattern = "dir")
  dir.create(t_dir)
  gen_file=paste(t_dir, "Dockerfile",sep="/");gen_file
  
  maintainer = new("Maintainer", name="Matthias Hinz", email="matthias.m.hinz@gmail.com")
  dfile = dockerfile(from=NULL, maintainer = maintainer,to=gen_file)
  write.Dockerfile(dfile)
  
  control_file = system.file("simple_dockerfile/Dockerfile", package = "containerit")
  control_file
  
  control_instructions = readLines(control_file)
  generated_instructions = readLines(gen_file)
  
  
  expect_equal(control_instructions, generated_instructions)
  
  unlink(t_dir,recursive = TRUE)
})

