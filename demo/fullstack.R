#' # Demo of full R-based container interaction with containerit and harbor

# devtools::install_github("wch/harbor")
library("harbor")
# devtools::install_github("o2r-project/containerit")
library("containerit")

#' The following code encapsulates a simple analysis of the cars dataset into a Docker container and executes the container.

#' Save a dataset as CSV to an imaginary workspace directory
workspace <- tempdir()
datafile <- file.path(workspace, "dataset.csv")
write.csv(file = datafile, x = cars)

#' Create a minimal analysis script
scriptfile <- file.path(workspace, "script.R")
fileConn<-file(scriptfile)
writeLines(c(
    'dataset <- read.csv("dataset.csv")',
    'model <- lm(log(dist) ~ log(speed), data = dataset)',
    'summary(model)'
  ),
  fileConn)
close(fileConn)

#' Create Dockerfile workspace with containerit
setwd(workspace)
cmd <- CMD_Rscript("script.R")
df <- containerit::dockerfile(from = workspace, cmd = cmd, r_version = "3.3.3", copy = "script_dir")
save(df)

#' Create image with harbor (see also utility function `containerit::docker_build(..)`).
buildresult <- harbor::docker_cmd(harbor::localhost, "build",
                                  arg = workspace,
                                  docker_opts = c("-t", "fullstack-r-demo"),
                                  capture_text = TRUE
)
cat(buildresult)
# equivalent to containerit::docker_build(dockerfolder = workspace, tag = "fullstack-r-demo")

#' Run the image locally with harbor
harbor::docker_run(image = "fullstack-r-demo")

#' ------

# devtools::install_github("cboettig/dockermachine")
library("dockermachine")

#' Spin up local machine with docker-machine and activate it with a system command.
dockermachine::machine_create("virtualbox")
dockermachine::machine_ls()

system2(command = "eval", args = c("$(", "docker-machine", "env", "machine", ")"))
system2(command = "eval", args = c("$(docker-machine env machine)"))
system2(command = "echo", args = c("$DOCKER_HOST"))
system2(command = "echo", args = c("test"))

system("eval $(docker-machine env machine)")

# FIXME!

#' Run the image in the machine with harbor
harbor::docker_run(harbor::localhost, "debian:testing", "echo foo")


#' Clean up
machine_stop()
machien_rm()

# knitr::spin("fullstack.R"); unlink("fullstack.md")
