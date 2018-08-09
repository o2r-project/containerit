# Copyright 2018 Opening Reproducible Research (https://o2r.info)

context("Save workspace and R objects (save_image - argument)")

original_sessionInfo <- obtain_localSessionInfo()
#this step is necessary because when running in test mode, the object is normally not written to the global environment
#alternatively, the second test could use a different envir-argument
assign("original_sessionInfo", original_sessionInfo, envir = .GlobalEnv)

test_that("A workspace image can be containerized", {
  expect_false(
    file.exists(".RData"),
    "RData file already exists in testthat folder. Remove manually and restart test."
  )
  if (file.exists(".RData"))
    return() #don't continue

  df <- dockerfile(original_sessionInfo, save_image = TRUE)

  expect_true(
    file.exists(".RData"),
    "The expected workspace image '.RData' file was not writen dor working directory."
  )
  attach(".RData")
  expect_true("original_sessionInfo" %in% ls(pos = 2) ,
              " R object was not saved to RData file.")
  detach(pos = 2)

  inst <- methods::slot(df, "instructions")
  inst_types <- sapply(inst, class)
  #select last occurence of a Workdir instruction
  last_wd_sel <- max(which(inst_types == "Workdir"))
  #select copy instructions that occure after workdir instructions
  copy_sel <- which(inst_types == "Copy")
  copy_sel <- copy_sel[copy_sel > last_wd_sel]

  expected_instruction <- toString(Copy("./.RData", "./"))
  generated_instructions <- sapply(inst[copy_sel], toString)
  expect_true(
    expected_instruction %in% generated_instructions,
    paste(
      "Expected a copy instruction after setting the workdir as follows:",
      expected_instruction
    )
  )
  #if you test-build the dockerfile, you should find the object original_sessionInfo loaded into the workspace
  unlink(".RData")
})


test_that("A workspace image can be containerized given an object list and save-arguments",
          {
            test_folder <- basename(tempfile(pattern = "safe_image_test"))
            dir.create(test_folder)
            targetfile <-
              paste0(test_folder, "/testworkspace.RData")
            message("Workspace here: ", ls(all.names = TRUE, envir = .GlobalEnv))
            df <-
              dockerfile(original_sessionInfo,
                         save_image = list("original_sessionInfo", file = targetfile))

            save_image = list("original_sessionInfo", file = targetfile)

            expect_true(
              file.exists(targetfile),
              paste(
                "The expected workspace image ",
                targetfile ,
                ", was not written in working directory."
              )
            )
            attach(targetfile)
            expect_true("original_sessionInfo" %in% ls(pos = 2) ,
                        " R object was not saved to RData file.")
            detach(pos = 2)

            inst <- methods::slot(df, "instructions")
            inst_types <- sapply(inst, class)
            #select last occurence of a Workdir instruction
            last_wd_sel <- max(which(inst_types == "Workdir"))
            #select copy instructions that occure after workdir instructions
            copy_sel <- which(inst_types == "Copy")
            copy_sel <- copy_sel[copy_sel > last_wd_sel]

            expected_instruction <-
              toString(Copy(targetfile, targetfile))
            generated_instructions <-
              sapply(inst[copy_sel], toString)
            expect_true(
              expected_instruction %in% generated_instructions,
              paste(
                "Expected a copy instruction after setting the workdir as follows:",
                expected_instruction
              )
            )
            #if you test-build the dockerfile, you should find the object original_sessionInfo loaded into the workspace
            unlink(test_folder, recursive = TRUE)
          })
