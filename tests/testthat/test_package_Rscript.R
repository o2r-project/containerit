
context("Packaging R-Scripts and workspaces.")



test_that("the R script location is checked ",{
  expect_error(dockerfile("falseScriptLocation.R"))
})

test_that("an R script can be created with resources of the same folder ",{
  print("Script executed locally: ")
  df=dockerfile("simple_test_script_resources/simple_test.R", copy = "script_dir", cmd = CMD_Rscript("simple_test_script_resources/simple_test.R")
                , maintainer = Maintainer("matthiashinz"), r_version = "3.3.2")
  #test run (shoud be fast:)
  image = create_localDockerImage(df, use_context = TRUE)
  print("Script reproduced with Docker: ")
  harbor::docker_run(image = image, rm = TRUE)
  harbor::docker_cmd(harbor::localhost, "rmi", image)
  
  expected_file = readLines("simple_test_script_resources/Dockerfile")
  expect_equal(format(df), expected_file)
})


test_that("an workspace with one R script can be packaged ",{
  #This test should result in the same dockerfile as above:  
  df=dockerfile("simple_test_script_resources/", cmd = CMD_Rscript("simple_test_script_resources/simple_test.R")
                , maintainer = Maintainer("matthiashinz"), r_version = "3.3.2")

  expected_file = readLines("simple_test_script_resources/Dockerfile")
  expect_equal(format(df), expected_file)
})


test_that("a list of resources can be packaged ",{
  df=dockerfile("simple_test_script_resources/simple_test.R", 
                copy = c("simple_test_script_resources/simple_test.R",
                         "simple_test_script_resources/test_table.csv",
                         "simple_test_script_resources/test_subfolder/testresource"),
                maintainer = Maintainer("matthiashinz"),
                r_version = "3.3.2")

  expected_file = readLines("simple_test_script_resources/Dockerfile2")
  expect_equal(format(df), expected_file)
})



test_that("The gstat demo 'zonal' can be packaged ",{
  expect_true(requireNamespace("sp"))
  expect_true(requireNamespace("gstat"))
  
  expect_false(file.exists("Rplots.pdf"))
  df=dockerfile("test_script_gstat/zonal.R", cmd = CMD_Rscript("test_script_gstat/zonal.R")
                , maintainer = Maintainer("matthiashinz"), r_version = "3.3.2")
  expect_true(file.exists("Rplots.pdf"))
  expect_true(file.size("Rplots.pdf") > 10000)
  #test execution would be similar to the test above; will not be done because of slowlieness
  expected_file = readLines("test_script_gstat/Dockerfile") ## TODO: will not run yet
  expect_equal(format(df), expected_file)
  ##clean up:
  unlink("Rplots.pdf")
})

