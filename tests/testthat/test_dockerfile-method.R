# Copyright 2016 Opening Reproducible Research (http://o2r.info)

library(containerit)
context("dockerfile-generation")

maintainer = new("Maintainer", name="Matthias Hinz", email="matthias.m.hinz@gmail.com")

test_that("a basic dockerfile with maintainer can be generated",{
  t_dir=tempfile(pattern = "dir")
  dir.create(t_dir)
  gen_file=paste(t_dir, "Dockerfile",sep="/");gen_file
  dfile = dockerfile(from=NULL, maintainer = maintainer,to=gen_file)
  write.Dockerfile(dfile)
  control_file = system.file("simple_dockerfile/Dockerfile", package = "containerit")
  control_file
  control_instructions = readLines(control_file)
  generated_instructions = readLines(gen_file)
  expect_equal(control_instructions, generated_instructions)
  unlink(t_dir,recursive = TRUE)
})

test_that("users can specify the base image",{
  imagestr="rocker/r-ver:3.0.0"
  dfile1 = dockerfile(image=imagestr, maintainer = maintainer);dfile1
  expect_equal(slot(dfile1, "image"), imagestr)
  fromstr=paste("FROM", imagestr)
  expect_length(which(slot(dfile1,"instructions")== fromstr),1)
  
  #expect that costum image is prefered over R version
  dfile2 = dockerfile(image=imagestr, maintainer = maintainer, r_version = "3.1.0");dfile2
  expect_equal(slot(dfile2, "image"), imagestr)
  fromstr=paste("FROM", imagestr)
  expect_length(which(slot(dfile2,"instructions")== fromstr),1)
})

test_that("users can specify the R version",{
  versionstr="3.1.0"
  dfile2 = dockerfile(r_version = versionstr, maintainer = maintainer);dfile2
  expect_match(slot(dfile2, "image"), versionstr)
  expect_match(as.character(slot(dfile2,"instructions")), versionstr, all=FALSE)
  
  #expect am error if the user specifies an unsupported R version
  expect_error(dockerfile(r_version = "2.0"))
})


