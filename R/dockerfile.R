# Copyright 2016 Daniel Nüst
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' dockerfile
#'
#' Create a Dockerfile from the
#'
#' @param from the source of the information to construct the Dockerfile
#' @param to path and file name to save the Dockerfile to
#' @param env the environment that should be included in the image
#'
#' @return An object of class Dockerfile
#' @export
#'
#' @examples
#' dockerfile()
#'
#' @import futile.logger
dockerfile <- function(from = utils::sessionInfo(), to = paste0(getwd(), "/", "Dockerfile"), env = NULL) {
  flog.debug("Creating a new Dockerfile from %s to %s", from, to)
  .dockerfile <- NA
  .originalFrom <- class(from)

  if(inherits(x = from, "sessionInfo")) {
    .dockerfile <- dockerfileFromSession(session = from, to = to)
  } else if (inherits(x = from, "file")) {
    .dockerfile <- dockerfileFromFile(file = from, to = to)
  } else if(inherits(x = from, "character") && dir.exists(from)) {
    .dockerfile <- dockerfileFromWorkspace(path = from, to)
    .originalFrom <- from
  } else {
    stop("Unsupported 'from': ", class(from), from)
  }

  flog.info("Created Dockerfile at %s based on %s", to, .originalFrom)
  message("Created Dockerfile at", to, "based on", .originalFrom)
  return(.dockerfile)
}


dockerfileFromSession <- function(session, to) {
  return(NA)
}

dockerfileFromFile <- function(file, to) {
  return(NA)
}

dockerfileFromWorkspace <- function(path, to) {
  .rFiles <- dir(path = path, pattern = "\\.R$", full.names = TRUE, include.dirs = FALSE, recursive = TRUE)

  return(NA)
}
