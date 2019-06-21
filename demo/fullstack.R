#' # Demo of full R-based container interaction with containerit

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
the_dockerfile <- containerit::dockerfile(from = workspace, cmd = cmd, image = getImageForVersion("3.3.3"), copy = "script_dir")
save(the_dockerfile)

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

#' Clean up
machine_stop()
machine_rm()

# knitr::spin("fullstack.R"); unlink("fullstack.md")
