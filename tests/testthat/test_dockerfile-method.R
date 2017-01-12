# Copyright 2016 Opening Reproducible Research (http://o2r.info)

library(containerit)
context("dockerfile-generation")

test_that("a simple dockerfile object can be saved to file",{
  t_dir=tempfile(pattern = "dir")
  dir.create(t_dir)
  #one dockerfile is generated, one is fixed for comparism
  gen_file=paste(t_dir, "Dockerfile",sep="/");gen_file
  maintainer = new("Maintainer", name="Matthias Hinz", email="matthias.m.hinz@gmail.com")
  dfile = dockerfile(from=NULL, maintainer = maintainer)
  write.Dockerfile(dfile, file = gen_file)
  control_file = "./dockerfile-method-resources/simple_dockerfile"
  control_instructions = readLines(control_file)
  generated_instructions = readLines(gen_file)
  #compare generated file with permanent file
  expect_equal(control_instructions, generated_instructions)
  
  unlink(t_dir,recursive = TRUE)
})

test_that("users can specify the maintainer",{
  maintainer = new("Maintainer", name="Matthias Hinz", email="matthias.m.hinz@gmail.com")
  dfile = dockerfile(NULL, maintainer = maintainer);dfile
  
  #check maintainer slot content and class
  expect_is(slot(dfile, "maintainer"), "Maintainer")
  expect_equal(attr(class(slot(dfile, "maintainer")),"package"), "containerit")
  expect_equal(slot(slot(dfile, "maintainer"), "name"), "Matthias Hinz")
  expect_equal(slot(slot(dfile, "maintainer"), "email"), "matthias.m.hinz@gmail.com")
  
  #expect Maintainer instruction
  expect_equal(toString(maintainer), "MAINTAINER \"Matthias Hinz\" matthias.m.hinz@gmail.com")

})


test_that("users can specify the base image",{
  imagestr="rocker/r-ver:3.0.0"
  maintainer = new("Maintainer", name="Matthias Hinz", email="matthias.m.hinz@gmail.com")
  dfile1 = dockerfile(from=NULL, image=imagestr, maintainer = maintainer);dfile1
  expect_equal(slot(dfile1, "image"), imagestr)
  fromstr=paste("FROM", imagestr)
  expect_length(which(slot(dfile1,"instructions")== fromstr),1)
  
  #expect that custom image is preferred over R version argument
  dfile2 = dockerfile(from=NULL, image=imagestr, maintainer = maintainer, r_version = "3.1.0");dfile2
  expect_equal(slot(dfile2, "image"), imagestr)
  fromstr=paste("FROM", imagestr)
  expect_length(which(slot(dfile2,"instructions")== fromstr),1)
})

test_that("users can specify the R version",{
  versionstr="3.1.0"
  maintainer = new("Maintainer", name="Matthias Hinz", email="matthias.m.hinz@gmail.com")
  dfile = dockerfile(r_version = versionstr, maintainer = maintainer);dfile
  #check content of image and instructions slots
  expect_match(slot(dfile, "image"), versionstr)
  expect_match(as.character(slot(dfile,"instructions")), versionstr, all=FALSE)
  #expect am error if the user specifies an unsupported R version
  expect_error(dockerfile(r_version = "2.0"))
})


test_that("R version is the current version if not specified otherwise",{
  dfile = dockerfile(NULL)
  #expect that image string contains the current R version
  expect_match(slot(dfile, "image"), paste(R.Version()$major, R.Version()$minor, sep="."))
})


