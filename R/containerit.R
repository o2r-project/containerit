# Copyright 2017 Opening Reproducible Research (http://o2r.info)

#' containerit: A package for packaging R objects and their dependencies in software containers
#'
#' The containerit package provides one core function: \code{dockerfile}.
#'
#' @section Create Dockerfiles:
#' The \code{dockerfile} method create an R representation of a Dockerfile, which is a recipee
#' for a Docker container. The object can be serialized to a Dockerfile, which can be used to build
#' a runnable Docker image. \code{dockerfile} objects can be created based on R scripts and sessions.
#'
#' @docType package
#' @name containerit
#' @aliases containers, containerization
#' @importFrom methods new
#' @importFrom methods slot
#' @importFrom methods slot<-
#' @importFrom utils sessionInfo

NULL
